;;; wingman.el --- LLM-assisted code/text completion using llama.cpp -*- lexical-binding: t -*-

;; Copyright (C) 2025 Michael Russo

;; Author: Michael Russo
;; URL: https://github.com/mjrusso/wingman
;; Package-Requires: ((emacs "27.1") (request "0.3.2") (dash "2.19.0"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Wingman brings Copilot-style ghost-text completions to Emacs via inline FIM
;; ("fill in middle") completions, powered by the llama.cpp server.
;;
;; This package is a port of <https://github.com/ggml-org/wingman.vim>.

;;; Code:

(require 'dash)
(require 'json)
(require 'project)
(require 'request)
(require 'seq)
(require 'subr-x)

(defgroup wingman nil
  "Inline code and text completions using the llama.cpp server."
  :group 'convenience)

(defface wingman-overlay-face
  '((t :inherit shadow))
  "Face used for the inline completion (ghost text) overlay."
  :group 'wingman)

(defcustom wingman-llama-endpoint "http://127.0.0.1:8012/infill"
  "URL of the llama.cpp FIM server endpoint."
  :type 'string)

(defcustom wingman-llama-api-key nil
  "Optional bearer token for the llama.cpp server."
  :type '(choice (const nil) string))

(defcustom wingman-n-prefix 256
  "How many lines before point are sent as local prefix."
  :type 'integer)

(defcustom wingman-n-suffix 64
  "How many lines after point are sent as local suffix."
  :type 'integer)

(defcustom wingman-n-predict 128
  "Maximum tokens to generate."
  :type 'integer)

(defcustom wingman-stop-strings nil
  "List of stop strings."
  :type '(repeat string))

(defcustom wingman-auto-fim t
  "If non-nil, request a completion automatically while typing."
  :type 'boolean)

(defcustom wingman-max-line-suffix 8
  "Do not trigger auto FIM if there are more than this many chars to the right of point."
  :type 'integer)

(defconst wingman--default-disable-predicates
  '(;; Disable in magit buffers, which are highly specialized.
    (lambda () (derived-mode-p 'magit-mode))
    ;; Disable in terminal emulator buffers.
    (lambda () (or (derived-mode-p 'vterm-mode)
                   (derived-mode-p 'shell-mode)
                   (derived-mode-p 'term-mode)
                   (derived-mode-p 'eshell-mode)))
    ;; Disable in special Emacs buffers that typically start with a "*".
    (lambda () (string-prefix-p "*" (buffer-name)))
    ;; Disable in read-only buffers where the user can't insert text anyway.
    (lambda () buffer-read-only)
    ;; Disable in help, info, and man page buffers.
    (lambda () (or (derived-mode-p 'help-mode)
                   (derived-mode-p 'Info-mode)
                   (derived-mode-p 'man-mode)))
    ;; Disable in compilation output buffers.
    (lambda () (derived-mode-p 'compilation-mode)))
  "A list of default predicates to decide whether `wingman-mode' should be disabled.
This is an internal variable. Users should customize `wingman-disable-predicates`
to add their own rules.")


(defcustom wingman-disable-predicates nil
  "A list of predicates to decide whether `wingman-mode' should be disabled.

This is checked in addition to a set of built-in predicates that disable
wingman in common non-editing buffers (e.g., Magit, vterm, read-only
buffers, etc.).

Each element must be a function that takes no arguments. If any
function in the list returns a non-nil value when called in a
buffer, `wingman-mode' will be disabled for that buffer.

This is useful for preventing sensitive content (e.g., files
with API keys or environment variables) from being sent to the
completion server.

Example:
(add-to-list 'wingman-disable-predicates
             (lambda ()
               (when buffer-file-name
                 (let ((fname (file-name-nondirectory buffer-file-name)))
                   (or (string-equal \\\".env\\\" fname)
                       (derived-mode-p 'envrc-mode))))))"
  :type '(repeat function)
  :group 'wingman)

(defcustom wingman-prefix-key "C-c w"
  "Key in the `wingman-mode-map' prefixing the `wingman-mode-prefix-map'."
  :type '(choice (const :tag "Unbound" nil) key)
  :group 'wingman)

(defcustom wingman-ring-n-chunks 16 "Maximum extra chunks." :type 'integer)
(defcustom wingman-ring-chunk-size 64 "Lines per chunk." :type 'integer)
(defcustom wingman-ring-scope 1024  "How far around point to harvest." :type 'integer)
(defcustom wingman-ring-update-ms 1000 "Background update cadence." :type 'integer)

(defgroup wingman-debug nil
  "Debugging options for wingman.el"
  :group 'wingman)

(defcustom wingman-log-level 0
  "0 = silent, 1 = errors, 2 = normal log, 3 = verbose, 4 = trace."
  :type '(choice
          (const :tag "Silent" 0)
          (const :tag "Errors" 1)
          (const :tag "Normal" 2)
          (const :tag "Verbose" 3)
          (const :tag "Trace" 4)))

(defcustom wingman-log-buffer "*wingman-log*"
  "Name of the buffer used to hold debugging messages."
  :type 'string)

(defcustom wingman-log-max-lines 500
  "Truncate the log buffer after this many lines (FIFO)."
  :type 'integer)

(defun wingman--log (level fmt &rest args)
  "Write a log entry if LEVEL is not greater than `wingman-log-level'."
  (when (>= wingman-log-level level)
    (let* ((msg (apply #'format fmt args))
           (buf (get-buffer-create wingman-log-buffer)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format-time-string "[%H:%M:%S] ") msg "\n")
        (when (> (count-lines (point-min) (point-max)) wingman-log-max-lines)
          (goto-char (point-min))
          (forward-line (- (count-lines (point-min) (point-max))
                           wingman-log-max-lines))
          (delete-region (point-min) (point)))))))

(cl-defstruct wingman--chunk data string timestamp filename project-root)

(defvar wingman--ring-chunks nil "List of processed context chunks ready to be sent to the server.")
(defvar wingman--ring-queue  nil "List of context chunks waiting for background processing.")

(defvar-local wingman--ring-evict 0)

(defvar wingman--cache (make-hash-table :test 'equal)) ;; sha256 → raw JSON

(defvar-local wingman--hint-overlay nil)
(defvar-local wingman--info-overlay nil)

(defvar-local wingman--current-request nil)
(defvar-local wingman--debounce-timer nil "Timer for debouncing auto-FIM requests.")
(defvar-local wingman--last-move-time (current-time))

(defvar-local wingman--content-lines nil "Content lines most recently completed from the server.")

(defconst wingman--marker "Î") ;; same Unicode char used in llama.vim

(defvar wingman--active-buffers nil
  "List of live buffers where `wingman-mode' is enabled.")

(defvar wingman--accepting-completion-p nil
  "Dynamically-scoped guard to prevent hooks from firing during completion acceptance.")

(defvar wingman--ring-timer nil)

(defun wingman--ensure-timer ()
  "Make sure the background ring-update timer is running."
  (unless (timerp wingman--ring-timer)
    (setq wingman--ring-timer
          (run-at-time 0 (/ wingman-ring-update-ms 1000.0)
                       #'wingman--ring-update-dispatch))))

(defun wingman--cancel-timer-if-unused ()
  "Cancel background timer when there are no active buffers."
  (when (and (timerp wingman--ring-timer)
             (null (seq-filter #'buffer-live-p wingman--active-buffers)))
    (cancel-timer wingman--ring-timer)
    (setq wingman--ring-timer nil)))

(defun wingman--should-be-disabled-p ()
  "Return t if any built-in or user-defined disable predicate is true."
  (let ((inhibit-message t))
    (or (cl-some #'funcall wingman--default-disable-predicates)
        (cl-some #'funcall wingman-disable-predicates))))

(defun wingman--pick-chunk-on-save ()
  "Hook function for `after-save-hook' to pick a chunk."
  (wingman--pick-chunk t))

(defun wingman--pick-chunk-on-yank ()
  "Hook function for `yank-post-process-hook' to pick a chunk."
  (wingman--pick-chunk))

(defvar-keymap wingman-mode-prefix-map
  :doc "Local map for wingman-mode. Will be prefixed by `wingman-prefix-key' in
the `wingman-mode-map' map."
  "TAB" #'wingman-fim-inline
  "d" #'wingman-debug-completion)

(defvar wingman-mode-map
  (let ((map (make-sparse-keymap)))
    (when wingman-prefix-key
      (define-key map (kbd wingman-prefix-key) wingman-mode-prefix-map))
    map)
  "Keymap for wingman-mode. Keybindings will use the prefix as defined by
`wingman-prefix-key'.")

(defvar-keymap wingman-mode-completion-transient-map
  :doc "Local map for wingman-mode while there is an active completion."
  "TAB" #'wingman-accept-full
  "S-TAB" #'wingman-accept-line
  "M-S-TAB" #'wingman-accept-word)

;;;###autoload
(define-minor-mode wingman-mode
  "Toggle inline LLM-assisted code completions."
  :lighter " 🦙"
  :keymap wingman-mode-map
  (wingman--log 2 "wingman-mode %s in buffer %s" (if wingman-mode "enabled" "disabled") (buffer-name))
  (if wingman-mode
      (if (wingman--should-be-disabled-p)
          (progn
            (message "wingman-mode: Disabled in this buffer due to configuration.")
            (wingman-mode -1))
        (progn
          (cl-pushnew (current-buffer) wingman--active-buffers)
          (add-hook 'post-command-hook #'wingman--on-point-move nil t)
          (add-hook 'after-save-hook #'wingman--pick-chunk-on-save nil t)
          (add-hook 'yank-post-process-hook #'wingman--pick-chunk-on-yank nil t)
          (add-hook 'kill-buffer-hook #'wingman--cleanup nil t)
          (run-at-time 0.1 nil (lambda () (wingman--pick-chunk t)))
          (wingman--ensure-timer)))
    (progn
      (setq wingman--active-buffers (delq (current-buffer) wingman--active-buffers))
      (remove-hook 'post-command-hook #'wingman--on-point-move t)
      (remove-hook 'after-save-hook #'wingman--pick-chunk-on-save t)
      (remove-hook 'yank-post-process-hook #'wingman--pick-chunk-on-yank t)
      (remove-hook 'kill-buffer-hook #'wingman--cleanup t)
      (wingman--cleanup))))

(defun wingman--ring-update-dispatch ()
  "Called by the single global timer to process the global chunk queue."
  (wingman--ring-update)
  (wingman--cancel-timer-if-unused))

;;;###autoload
(defun wingman-mode-enable ()  (interactive) (wingman-mode  1))
;;;###autoload
(defun wingman-mode-disable () (interactive) (wingman-mode -1))
;;;###autoload
(defun wingman-mode-toggle ()  (interactive) (if wingman-mode (wingman-mode-disable) (wingman-mode-enable)))

(defun wingman--indent-of (string)
  "Return indentation width (counting TAB as `tab-width')."
  (if (string-match "\\`\\([\t ]+\\)" string)
      (let ((spaces 0))
        (dolist (ch (string-to-list (match-string 1 string)) spaces)
          (setq spaces (+ spaces (if (eq ch ?\t) (- tab-width (mod spaces tab-width)) 1)))))
    0))

(defun wingman--collect-local-context (&optional prev)
  "Return an alist (prefix middle suffix indent line-prefix line-suffix line-full)."
  (save-excursion
    (let ((pos (point))
          (cur-line (or (thing-at-point 'line t) ""))
          (line-no (line-number-at-pos))
          lines-prefix lines-suffix prefix middle suffix indent
          line-prefix line-suffix)
      (setq line-prefix (buffer-substring (line-beginning-position) pos)
            line-suffix (buffer-substring pos (line-end-position)))

      (setq lines-prefix
            (wingman--buffer-lines
             (max 1 (- line-no wingman-n-prefix)) (1- line-no)))
      (setq lines-suffix
            (wingman--buffer-lines
             (1+ line-no) (min (line-number-at-pos (point-max))
                               (+ line-no wingman-n-suffix))))

      (if (or (null prev) (not (listp prev)))
          (setq indent (wingman--indent-of cur-line))
        (progn
          (setq indent (car-safe prev))
          (setq lines-prefix
                (append (last lines-prefix (- (length prev) 1))
                        prev))
          (setq line-prefix (concat cur-line (car prev)))
          (setq line-suffix "")))

      (setq prefix  (concat (string-join lines-prefix "\n") "\n")
            middle  line-prefix
            suffix  (concat line-suffix "\n"
                            (string-join lines-suffix "\n") "\n"))

      `((prefix . ,prefix)
        (middle . ,middle)
        (suffix . ,suffix)
        (indent . ,indent)
        (line-prefix . ,line-prefix)
        (line-suffix . ,line-suffix)
        (line-full  . ,cur-line)))))

(defun wingman--buffer-lines (from to)
  "Return a list of lines between line numbers FROM and TO inclusive."
  (when (<= from to)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- from))
      (let ((beg (point)))
        (goto-char (point-min))
        (forward-line (1- to))
        (let ((end (line-end-position)))
          (split-string (buffer-substring-no-properties beg end) "\n"))))))

(defun wingman-fim-inline ()
  "Manual trigger key. Hides an existing hint, or fetches a new one."
  (interactive)
  (wingman--log 4 "wingman-fim-inline called")
  (if (overlayp wingman--hint-overlay)
      (progn
        (wingman--log 3 "Hiding existing overlay")
        (wingman-hide))
    (progn
      (wingman--log 3 "No existing overlay, requesting FIM")
      (wingman--fim nil))))

(defun wingman--on-point-move ()
  "Hide hint on movement; possibly auto-trigger a new one."
  (unless (or wingman--accepting-completion-p
              (and (symbolp this-command)
                   (string-prefix-p "wingman-" (symbol-name this-command))))
    (setq wingman--last-move-time (current-time))

    (when (or (overlayp wingman--hint-overlay) (overlayp wingman--info-overlay))
      (wingman--log 3 "Hiding overlay due to point movement (command: %s)" this-command)
      (wingman-hide))

    (when (and wingman-auto-fim (eq this-command 'self-insert-command))
      (let ((suffix-len (length (buffer-substring-no-properties (point) (line-end-position)))))
        (wingman--log 4 "Auto-FIM check: suffix-len=%d, max=%d" suffix-len wingman-max-line-suffix)
        (when (<= suffix-len wingman-max-line-suffix)
          (wingman--log 3 "Auto-triggering FIM")
          (wingman--fim t))))))

(defun wingman--sha256 (string)
  (secure-hash 'sha256 string nil nil t))

(defun wingman--cache-get (hash) (gethash hash wingman--cache))
(defun wingman--cache-put (hash raw) (puthash hash raw wingman--cache))

(defun wingman--string-common-prefix (s1 s2)
  "Return the common prefix of strings S1 and S2."
  (let ((i 0)
        (len (min (length s1) (length s2))))
    (while (and (< i len) (eq (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun wingman--fim (auto)
  "Main completion routine. If AUTO is non-nil, then this is an automatic trigger."
  (cl-block wingman--fim
    (wingman--log 3 "wingman--fim called (auto=%s)" auto)

    (when (and wingman--current-request auto)
      (wingman--log 3 "Debouncing: request in flight.")

      (when (timerp wingman--debounce-timer)
        (cancel-timer wingman--debounce-timer))

      (setq wingman--debounce-timer
            (run-at-time 0.1 nil #'wingman--fim auto))

      (cl-return-from wingman--fim))

    ;; If we are proceeding, it means no request is in-flight.
    ;; We must cancel any pending debounce timer that might have been
    ;; scheduled before the previous request completed.
    (when (timerp wingman--debounce-timer)
      (cancel-timer wingman--debounce-timer)
      (setq wingman--debounce-timer nil))

    (let* ((ctx  (wingman--collect-local-context))
           (pre  (alist-get 'prefix ctx))
           (mid  (alist-get 'middle ctx))
           (suf  (alist-get 'suffix ctx))
           (indent (alist-get 'indent ctx))
           (hash  (wingman--sha256 (concat pre mid wingman--marker suf))))

      (wingman--log 3 "Context: prefix=%d chars, middle='%s', suffix=%d chars"
                    (length pre) (truncate-string-to-width mid 20 nil nil t) (length suf))
      (if-let ((cached (wingman--cache-get hash)))
          (progn
            (wingman--log 2 "Cache HIT for hash %s" (substring hash 0 8))
            (wingman--render cached indent (current-buffer)))
        (progn
          (wingman--log 2 "Cache MISS for hash %s - making HTTP request" (substring hash 0 8))
          (wingman--http-request ctx indent (list hash) (current-buffer)))))))

(defun wingman--http-request (ctx indent hashes origin-buffer)
  "Send asynchronous HTTP request; store HANDLE in `wingman--current-request'."
  (wingman--log 2 "HTTP → prefix:%d chars, suffix:%d chars, hashes:%d"
                (length (alist-get 'prefix ctx))
                (length (alist-get 'suffix ctx))
                (length hashes))
  (when wingman--current-request
    (request-abort wingman--current-request))
  (let* ((payload
          `(("input_prefix" . ,(alist-get 'prefix ctx))
            ("input_suffix" . ,(alist-get 'suffix ctx))
            ("input_extra"  . ,(wingman--extra-context ctx))
            ("prompt"       . ,(alist-get 'middle ctx))
            ("n_predict"    . ,wingman-n-predict)
            ("stop"         . ,wingman-stop-strings)
            ("n_indent"     . ,indent)
            ("top_k"        . 40)
            ("top_p"        . 0.9)
            ("stream"       . :json-false)
            ("samplers"     . ["top_k" "top_p" "infill"])
            ("cache_prompt" . t)
            ("response_fields" . ["content"
                                  "tokens_cached"
                                  "timings/prompt_n"
                                  "timings/prompt_ms"
                                  "timings/predicted_n"
                                  "timings/predicted_ms"])))
         (url wingman-llama-endpoint)
         (headers (append '(("Content-Type" . "application/json"))
                          (when wingman-llama-api-key
                            `(("Authorization" . ,(concat "Bearer " wingman-api-key))))))
         (data (json-encode payload)))
    (setq wingman--current-request
          (request
            url
            :type "POST"
            :headers headers
            :data data
            :parser 'buffer-string
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (if (and (buffer-live-p origin-buffer)
                                 (eq (current-buffer) origin-buffer))
                            (progn
                              (wingman--log 2 "HTTP ← %d bytes, caching under %d hashes"
                                            (length data) (length hashes))
                              (dolist (h hashes)
                                (wingman--cache-put h data))
                              (wingman--render data indent origin-buffer))
                          (cond
                           ((not (buffer-live-p origin-buffer))
                            (wingman--log 3 "Ignoring stale completion: origin buffer '%s' was killed."
                                          (buffer-name origin-buffer)))
                           ((not (eq (current-buffer) origin-buffer))
                            (wingman--log 3 "Ignoring stale completion: buffer changed from '%s' to '%s'."
                                          (buffer-name origin-buffer)
                                          (buffer-name (current-buffer))))))))
            :error (cl-function
                    (lambda (&rest args &key error-thrown &allow-other-keys)
                      (wingman--log 1 "HTTP ERROR: %S" error-thrown)))
            :complete (lambda (&rest _) (setq wingman--current-request nil))))))

(defun wingman--extra-context (&optional ctx)
  "Return a vector of alists representing chunks from the ring buffer.
If the optional `ctx` alist is provided (during a FIM request), this
function filters out chunks from the ring buffer that are too similar to
the current local context. Eliminating the overlap between the local FIM
context and the ring buffer context reduces the tendency of the
completion model to generate redundant completions.

If `ctx` is nil (e.g., during background cache priming), no similarity
filtering is performed."
  (let* ((current-project-root (when-let ((proj (project-current)))
                                 (project-root proj)))
         (project-chunks (-filter (lambda (c)
                                    (equal (wingman--chunk-project-root c)
                                           current-project-root))
                                  wingman--ring-chunks))
         (filtered-chunks
          (if ctx
              (let* ((local-context-lines (append (split-string (alist-get 'prefix ctx "") "\n")
                                                  (split-string (alist-get 'suffix ctx "") "\n")))
                     (local-chunk (make-wingman--chunk :data local-context-lines))
                     (chunks-after-sim-filter (seq-remove (lambda (existing-chunk)
                                                            (> (wingman--chunk-similarity existing-chunk local-chunk) 0.5))
                                                          project-chunks))
                     (evicted-count (- (length project-chunks) (length chunks-after-sim-filter))))

                (when (> evicted-count 0)
                  (wingman--log 3 "Ring: temporarily filtering %d chunks similar to the current context for this request"
                                evicted-count))
                chunks-after-sim-filter)
            project-chunks)))

    (wingman--log 3 "Sending %d chunks for project: %s"
                  (length filtered-chunks) (or current-project-root "global"))

    (vconcat
     (mapcar (lambda (c)
               `((text . ,(wingman--chunk-string c))
                 (time . ,(wingman--chunk-timestamp c))
                 (filename . ,(wingman--chunk-filename c))))
             filtered-chunks))))

(defun wingman--render (raw indent buf)
  "Display RAW JSON as ghost text overlay, handling partial text."
  (cl-block wingman--render
    (let ((resp (ignore-errors (json-read-from-string raw))))
      (unless resp
        (wingman--log 1 "Failed to parse JSON response. Raw response: %s" raw)
        (cl-return-from wingman--render))

      (let* ((content-text (alist-get 'content resp ""))
             (content-lines (split-string content-text "\n")))

        (while (and content-lines (string-empty-p (car (last content-lines))))
          (setq content-lines (butlast content-lines)))

        (when (or (null content-lines)
                  (and (= (length content-lines) 1)
                       (string-empty-p (car content-lines))))
          (wingman--log 3 "Received empty or no content from server. Aborting render.")
          (cl-return-from wingman--render))

        (wingman-hide) ; Clear previous overlays.

        (let* ((pos (point))
               (current-suffix (buffer-substring-no-properties pos (line-end-position)))
               (first-line (car content-lines))
               (remaining-lines (cdr content-lines))
               (accept-content-lines (copy-sequence content-lines))

               ;; Find what part of the completion is *not* already typed.
               (common-prefix (wingman--string-common-prefix first-line current-suffix))
               (display-first-line (substring first-line (length common-prefix)))

               ;; Create a zero-width overlay at point for the first line hint.
               (first-ov (make-overlay pos pos buf))
               ;; Create a second overlay for any subsequent lines.
               (multi-ov (when remaining-lines (make-overlay pos pos buf))))

          ;; If there is nothing new to display on the first line and no other lines, do nothing.
          (when (or (not (string-empty-p display-first-line)) remaining-lines)
            (wingman--log 2 "Rendering hint: %s... (%d lines)"
                          (truncate-string-to-width (car content-lines) 50)
                          (length content-lines))

            ;; Append the existing text (after the cursor) to the last line of
            ;; the suggestion. See: <https://github.com/mjrusso/wingman/issues/2>
            (when (and accept-content-lines (not (string-empty-p current-suffix)))
              (setcar (last accept-content-lines)
                      (concat (car (last accept-content-lines)) current-suffix)))

            (overlay-put first-ov 'after-string (propertize display-first-line 'face 'wingman-overlay-face))
            (overlay-put first-ov 'wingman t)
            (setq wingman--hint-overlay first-ov)

            (when multi-ov
              (let ((multi-display
                     (concat "\n"
                             (mapconcat (lambda (line) (propertize line 'face 'wingman-overlay-face))
                                        remaining-lines
                                        "\n"))))
                (overlay-put multi-ov 'after-string multi-display)
                (overlay-put multi-ov 'wingman t)
                (setq wingman--info-overlay multi-ov)))
            (setq wingman--content-lines accept-content-lines)
            (set-transient-map wingman-mode-completion-transient-map t)))))))

(defun wingman-hide ()
  "Clear existing hint overlays and detach accept keys."
  (interactive)
  (when (overlayp wingman--hint-overlay)
    (delete-overlay wingman--hint-overlay)
    (setq wingman--hint-overlay nil))
  (when (overlayp wingman--info-overlay)
    (delete-overlay wingman--info-overlay)
    (setq wingman--info-overlay nil))
  (setq wingman--content-lines nil))

(defun wingman-accept-full ()
  "Accept the full suggestion."
  (interactive)
  (let ((wingman--accepting-completion-p t))
    (wingman--accept 'full wingman--content-lines)))

(defun wingman-accept-line ()
  "Accept the first line of the suggestion."
  (interactive)
  (let ((wingman--accepting-completion-p t))
    (wingman--accept 'line wingman--content-lines)))

(defun wingman-accept-word ()
  "Accept the first word of the suggestion."
  (interactive)
  (let ((wingman--accepting-completion-p t))
    (wingman--accept 'word wingman--content-lines)))

(defun wingman--accept (how content-lines)
  "Insert completion CONTENT-LINES according to HOW and remove overlay."
  (wingman-hide)
  (if content-lines
      (let* ((pos (point))
             (bol (line-beginning-position))
             (eol (line-end-position))
             (prefix (buffer-substring-no-properties bol pos)))
        (combine-after-change-calls
          (wingman--log 2 "Accepted %s (%d lines)" how (length content-lines))
          (pcase how
            ('word
             (let* ((first-line-suggestion (car content-lines))
                    (original-suffix (buffer-substring-no-properties pos eol))
                    (suggestion-for-word (if (string-suffix-p original-suffix first-line-suggestion)
                                             (substring first-line-suggestion 0 (- (length first-line-suggestion) (length original-suffix)))
                                           first-line-suggestion))
                    (first-word (car (split-string suggestion-for-word "\\b" t))))
               (delete-region bol eol)
               (insert (concat prefix first-word original-suffix))
               (goto-char (+ bol (length prefix) (length first-word)))))
            ('line
             (delete-region bol eol)
             (insert (concat prefix (car content-lines))))
            ('full
             (delete-region bol eol)
             (insert (concat prefix (string-join content-lines "\n")))))))
    (wingman--log 2 "No lines to accept")))

(defun wingman--random-chunk (text)
  "Return a random slice of TEXT (list of lines), max size ring_chunk_size/2."
  (let* ((len (length text))
         (max-chunk-size (/ wingman-ring-chunk-size 2))
         (start (if (> len max-chunk-size) (random (- len max-chunk-size)) 0)))
    (seq-subseq text start (min len (+ start max-chunk-size)))))

(defun wingman--pick-chunk (&optional around-point)
  "Select a chunk and store in the global queue, tagged with its project."
  (when (> wingman-ring-n-chunks 0)
    (let* ((lines (if around-point
                      (wingman--buffer-lines
                       (max 1 (- (line-number-at-pos) (/ wingman-ring-chunk-size 2)))
                       (min (line-number-at-pos (point-max))
                            (+ (line-number-at-pos) (/ wingman-ring-chunk-size 2))))
                    (wingman--buffer-lines 1 (line-number-at-pos (point-max)))))
           (chunk (wingman--random-chunk lines))
           (chunk-str (concat (string-join chunk "\n") "\n"))
           (project-root (when-let ((proj (project-current)))
                           (project-root proj)))
           (new-chunk (make-wingman--chunk :data chunk
                                           :string chunk-str
                                           :timestamp (float-time)
                                           :filename (or (buffer-file-name) (buffer-name))
                                           :project-root project-root)))
      ;; Evict similar chunks before adding the new one
      (wingman--evict-similar-chunks new-chunk 0.5)
      (wingman--log 3 "Queued chunk of %d lines from %s (project: %s)"
                    (length chunk) (buffer-name) (or project-root "global"))
      (push new-chunk wingman--ring-queue))))

(defun wingman--chunk-similarity (chunk1 chunk2)
  "Compute similarity between two chunks (0.0 to 1.0)."
  (let* ((lines1 (wingman--chunk-data chunk1))
         (lines2 (wingman--chunk-data chunk2))
         (len1 (length lines1))
         (len2 (length lines2))
         (common 0))
    (dolist (line1 lines1)
      (when (member line1 lines2)
        (setq common (1+ common))))
    (if (= (+ len1 len2) 0) 0.0
      (/ (* 2.0 common) (+ len1 len2)))))

(defun wingman--evict-similar-chunks (new-chunk threshold)
  "Remove chunks from ring that are too similar to NEW-CHUNK."
  (setq wingman--ring-chunks
        (seq-remove (lambda (existing)
                      (> (wingman--chunk-similarity existing new-chunk) threshold))
                    wingman--ring-chunks)))

(defun wingman--ring-update ()
  "Move one queued chunk from the global queue to the global processed list
and ping server so it is cached."
  (when (and wingman--ring-queue (< (length wingman--ring-chunks) wingman-ring-n-chunks))
    (wingman--log 3 "Ring update: %d queued → %d active"
                  (length wingman--ring-queue)
                  (length wingman--ring-chunks))
    (push (pop wingman--ring-queue) wingman--ring-chunks)

    (ignore-errors
      ;; zero-token request so the server puts it in its KV-cache
      (request
       wingman-llama-endpoint
       :type "POST"
       :data (json-encode
              `(("input_prefix" . "")
                ("input_suffix" . "")
                ("input_extra"  . ,(wingman--extra-context))
                ("prompt"       . "")
                ("n_predict"    . 0)
                ("stream" . :json-false)
                ("samplers" . [])))
       :parser 'ignore
       :error (lambda (&rest _) (message "wingman: background prime failed (server offline?)"))))))

(add-hook 'after-init-hook #'wingman--ring-update)

(defun wingman--cleanup ()
  "Buffer-local cleanup when wingman-mode is disabled or buffer is killed."
  (wingman-hide)
  (wingman--cancel-timer-if-unused)
  (when wingman--current-request
    (request-abort wingman--current-request))
  (setq wingman--ring-evict 0)
  (setq wingman--content-lines nil))

(defun wingman-clear-cache ()
  "Clear the in-memory cache of FIM responses."
  (interactive)
  (clrhash wingman--cache)
  (wingman--log 2 "Cache cleared.")
  (message "Wingman cache cleared."))

(defun wingman-clear-ring-buffer ()
  "Clear the global ring buffer and queue."
  (interactive)
  (setq wingman--ring-chunks nil
        wingman--ring-queue nil)
  (wingman--log 2 "Global ring buffer cleared.")
  (message "Wingman ring buffer cleared."))

(defun wingman-clear-all ()
  "Clear the in-memory cache of FIM responses and the global ring buffer and queue."
  (interactive)
  (wingman-clear-cache)
  (wingman-clear-ring-buffer))

(defun wingman-ring-report ()
  "Display the current state of the global ring buffer."
  (interactive)
  (if (null wingman--ring-chunks)
      (message "wingman: ring is empty")
    (with-current-buffer (get-buffer-create "*wingman-ring*")
      (erase-buffer)
      (dolist (c wingman--ring-chunks)
        (insert (format "• %s (%s)\n  (Project: %s, %d lines, %s)\n"
                        (file-name-nondirectory (wingman--chunk-filename c))
                        (file-name-directory (wingman--chunk-filename c))
                        (or (wingman--chunk-project-root c) "none")
                        (length (wingman--chunk-data c))
                        (format-time-string "%H:%M:%S"
                                            (seconds-to-time (wingman--chunk-timestamp c)))))
        (insert (propertize (string-join (wingman--chunk-data c) "\n")
                            'face 'shadow) "\n\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun wingman-open-log ()
  "Pop to the `wingman-log-buffer'."
  (interactive)
  (pop-to-buffer (get-buffer-create wingman-log-buffer)))


(defun wingman-debug-ring ()
  "Show current ring buffer status."
  (interactive)
  (message "Ring: %d chunks, %d queued. Chunks: %S"
           (length wingman--ring-chunks)
           (length wingman--ring-queue)
           (mapcar #'wingman--chunk-filename wingman--ring-chunks)))

(defun wingman--comment-multiline (text)
  "Add ;; comment prefix to each line of TEXT."
  (mapconcat (lambda (line) (concat ";; " line))
             (split-string text "\n")
             "\n"))

(defun wingman-debug-completion ()
  "Generate elisp code in a temp buffer for debugging completion requests."
  (interactive)
  (let* ((ctx (wingman--collect-local-context))
         (pre (substring-no-properties (alist-get 'prefix ctx)))
         (mid (substring-no-properties (alist-get 'middle ctx)))
         (suf (substring-no-properties (alist-get 'suffix ctx)))
         (indent (alist-get 'indent ctx))
         (extra-context (wingman--extra-context ctx))
         (buf (get-buffer-create "*wingman-debug-request*")))

    (with-current-buffer buf
      (erase-buffer)
      (insert ";;; wingman-request-debugger --- debug tool for wingman.el -*- lexical-binding: t -*-\n\n")
      (insert ";;; Commentary:\n\n")
      (insert ";; Evaluate this buffer to send a completion request and observe results\n\n\n")
      (emacs-lisp-mode)

      (insert ";; Current context:\n;;\n")
      (insert (format ";; - Prefix: %d chars\n" (length pre)))
      (insert (format ";; - Middle: %d chars\n" (length mid)))
      (insert (format ";; - Suffix: %d chars\n" (length suf)))
      (insert (format ";; - Ring buffer chunks: %d\n" (length extra-context)))
      (insert (format ";; - Indent: %d\n" indent))
      (insert "\n\n")

      (insert ";; Prefix content:\n;;\n;;```\n")
      (insert (wingman--comment-multiline pre))
      (insert "\n;;```\n\n\n")

      (insert ";; Middle content:\n;;\n;;```\n")
      (insert (wingman--comment-multiline mid))
      (insert "\n;;```\n\n\n")

      (insert ";; Suffix content:\n;;\n;;```\n")
      (insert (wingman--comment-multiline suf))
      (insert "\n;;```\n\n\n")

      (insert ";; Ring buffer contents (`input_extra`):\n;;\n")
      (if (= (length extra-context) 0)
          (insert ";; (empty)\n")
        (dotimes (i (length extra-context))
          (let* ((chunk (aref extra-context i))
                 (text (alist-get 'text chunk))
                 (filename (alist-get 'filename chunk))
                 (timestamp (alist-get 'time chunk)))
            (insert (format ";; Chunk %d: %s (%.2f)\n" (1+ i) filename timestamp))
            (insert (wingman--comment-multiline
                     (format "   Text: %S" (truncate-string-to-width text 80))))
            (insert "\n"))))
      (insert "\n\n\n")

      (insert "(setq debug-prefix ")
      (pp pre buf)
      (insert ")\n\n")

      (insert "(setq debug-suffix ")
      (pp suf buf)
      (insert ")\n\n")

      (insert "(setq debug-middle ")
      (pp mid buf)
      (insert ")\n\n")

      (insert "(setq debug-extra-context ")
      (pp extra-context buf)
      (insert ")\n\n")

      (insert ";; (setq debug-extra-context []) ; uncomment this line to disable ring buffer\n\n")

      (insert "(setq debug-n-predict ")
      (pp wingman-n-predict buf)
      (insert ")\n\n")

      (insert "(setq debug-stop-strings ")
      (pp wingman-stop-strings buf)
      (insert ")\n\n")

      (insert "(setq debug-n-indent ")
      (pp indent buf)
      (insert ")\n\n")

      (insert "(setq debug-url ")
      (pp wingman-llama-endpoint buf)
      (insert ")\n\n")

      (insert "(setq debug-headers '")
      (pp (append '(("Content-Type" . "application/json"))
                  (when wingman-llama-api-key
                    `(("Authorization" . ,(concat "Bearer " wingman-llama-api-key)))))
          buf)
      (insert ")\n\n")

      (insert ";; USAGE EXAMPLES:\n")
      (insert ";;\n")
      (insert ";; To disable ring buffer:\n")
      (insert ";; (setq debug-extra-context [])\n")
      (insert ";;\n")
      (insert ";; To test with modified prefix:\n")
      (insert ";; (setq debug-prefix \"your custom prefix here\")\n")
      (insert ";;\n")
      (insert ";; To test with different context:\n")
      (insert ";; (setq debug-middle \"    return a + b\")\n")
      (insert ";;\n")
      (insert ";; To add custom ring buffer content:\n")
      (insert ";; (setq debug-extra-context\n")
      (insert ";;       [{\"text\" \"def example():\\n    pass\\n\"\n")
      (insert ";;         \"time\" 1234567890.0\n")
      (insert ";;         \"filename\" \"example.py\"}])\n")
      (insert ";;\n")
      (insert ";; After modifying any variables, re-evaluate the request block below.\n\n")

      (insert "(require 'request)\n")
      (insert "(require 'json)\n\n")

      (insert "(let* ((payload (list\n")
      (insert "                 (cons \"input_prefix\" debug-prefix)\n")
      (insert "                 (cons \"input_suffix\" debug-suffix)\n")
      (insert "                 (cons \"input_extra\" debug-extra-context)\n")
      (insert "                 (cons \"prompt\" debug-middle)\n")
      (insert "                 (cons \"n_predict\" debug-n-predict)\n")
      (insert "                 (cons \"stop\" debug-stop-strings)\n")
      (insert "                 (cons \"n_indent\" debug-n-indent)\n")
      (insert "                 (cons \"top_k\" 40)\n")
      (insert "                 (cons \"top_p\" 0.9)\n")
      (insert "                 (cons \"stream\" :json-false)\n")
      (insert "                 (cons \"samplers\" [\"top_k\" \"top_p\" \"infill\"])\n")
      (insert "                 (cons \"cache_prompt\" t)\n")
      (insert "                 (cons \"response_fields\" [\"content\"\n")
      (insert "                                           \"tokens_cached\"\n")
      (insert "                                           \"timings/prompt_n\"\n")
      (insert "                                           \"timings/prompt_ms\"\n")
      (insert "                                           \"timings/predicted_n\"\n")
      (insert "                                           \"timings/predicted_ms\"])))\n")
      (insert "       (data (json-encode payload)))\n")
      (insert "  (request\n")
      (insert "   debug-url\n")
      (insert "   :type \"POST\"\n")
      (insert "   :headers debug-headers\n")
      (insert "   :data data\n")
      (insert "   :parser 'buffer-string\n")
      (insert "   :success (cl-function\n")
      (insert "             (lambda (&key data &allow-other-keys)\n")
      (insert "               (let* ((resp (json-read-from-string data))\n")
      (insert "                      (content (alist-get 'content resp))\n")
      (insert "                      (tokens-cached (alist-get 'tokens_cached resp))\n")
      (insert "                      (prompt-n (alist-get 'timings/prompt_n resp))\n")
      (insert "                      (prompt-ms (alist-get 'timings/prompt_ms resp))\n")
      (insert "                      (predicted-n (alist-get 'timings/predicted_n resp))\n")
      (insert "                      (predicted-ms (alist-get 'timings/predicted_ms resp)))\n")
      (insert "                 (message \"Completion received (%d chars)\" (length content))\n")
      (insert "                 (message \"Tokens cached: %s\" tokens-cached)\n")
      (insert "                 (message \"Prompt: %s tokens in %.1fms\" prompt-n prompt-ms)\n")
      (insert "                 (message \"Predicted: %s tokens in %.1fms\" predicted-n predicted-ms)\n")
      (insert "                 (with-current-buffer (get-buffer-create \"*wingman-debug-result*\")\n")
      (insert "                   (erase-buffer)\n")
      (insert "                   (insert \"Completion:\\n\\n```\\n\")\n")
      (insert "                   (insert content)\n")
      (insert "                   (insert \"\\n```\\n\\n\")\n")
      (insert "                   (insert \"Metadata:\\n\\n\")\n")
      (insert "                   (insert (format \"* Tokens cached: %s\\n\" tokens-cached))\n")
      (insert "                   (insert (format \"* Prompt: %s tokens in %.1fms\\n\" prompt-n prompt-ms))\n")
      (insert "                   (insert (format \"* Predicted: %s tokens in %.1fms\\n\" predicted-n predicted-ms))\n")
      (insert "                   (goto-char (point-min))\n")
      (insert "                   (display-buffer (current-buffer))))))\n")
      (insert "   :error (cl-function\n")
      (insert "           (lambda (&rest args &key error-thrown &allow-other-keys)\n")
      (insert "             (message \"Request failed: %S\" error-thrown)\n")
      (insert "             (with-current-buffer (get-buffer-create \"*wingman-debug-result*\")\n")
      (insert "               (erase-buffer)\n")
      (insert "               (insert \"Request failed:\\n\")\n")
      (insert "               (insert (format \"%S\" error-thrown))\n")
      (insert "               (display-buffer (current-buffer)))))))\n")
      (insert "\n")

      (goto-char (point-min))
      (display-buffer buf)

      (with-current-buffer buf
        (condition-case err
            (eval-buffer)
          (error
           (message "Debug evaluation failed: %S" err)))))))

(provide 'wingman)
;;; wingman.el ends here
