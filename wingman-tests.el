;;; wingman-tests.el --- Unit tests for wingman.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Michael Russo

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

;; Unit tests for wingman.el using ERT (Emacs Lisp Regression Testing).
;;
;; To run these tests, use the Makefile provided with the package, e.g., `make test`.

;;; Code:

(require 'wingman)
(require 'ert)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'cl-lib)

;;;; Mock Dependencies

(defun request (&rest _args)
  "Mock implementation of `request' to avoid actual network calls during tests."
  nil)

(defun wingman--log (level fmt &rest args)
  "Mock log function to intercept log messages for inspection during tests.
This prevents tests from being noisy and allows asserting on logged output."
  (when (and (boundp 'wingman-log-level) (>= wingman-log-level level))
    (let* ((msg (apply #'format fmt args))
           (buf-name (or (and (boundp 'wingman-log-buffer) wingman-log-buffer) "*test-wingman-log*"))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert msg "\n")))))


;;;; Test Cases

(ert-deftest test-wingman--sha256 ()
  "Test SHA256 hashing function."
  (should (stringp (wingman--sha256 "test")))
  (should (= (length (wingman--sha256 "test")) 32))
  (should (string-equal (wingman--sha256 "test") (wingman--sha256 "test")))
  (should (not (string-equal (wingman--sha256 "test") (wingman--sha256 "different")))))

(ert-deftest test-wingman--indent-of ()
  "Test indentation calculation function."
  (let ((tab-width 8)) ; Ensure a consistent tab-width for tests
    (should (= (wingman--indent-of "    hello") 4))
    (should (= (wingman--indent-of "\t\thello") 16))
    (should (= (wingman--indent-of "hello") 0))
    (should (= (wingman--indent-of "  \thello") 8)) ; 2 spaces, then tab aligns to col 8
    (should (= (wingman--indent-of "") 0))))

(ert-deftest test-wingman--truncate-line ()
  "Test line truncation functionality."
  (let ((wingman-ring-max-line-length 10)
        (wingman-log-level 0))
    (should (string-equal (wingman--truncate-line "short") "short"))
    (should (string-equal (wingman--truncate-line "this is a very long line that should be truncated")
                          "this is a "))
    (should (= (length (wingman--truncate-line "this is a very long line that should be truncated")) 10)))
  (let ((wingman-ring-max-line-length nil))
    (should (string-equal (wingman--truncate-line "this line should not be truncated regardless of length")
                          "this line should not be truncated regardless of length"))))

(ert-deftest test-wingman--string-common-prefix ()
  "Test string common prefix function."
  (should (string-equal (wingman--string-common-prefix "hello" "help") "hel"))
  (should (string-equal (wingman--string-common-prefix "abc" "xyz") ""))
  (should (string-equal (wingman--string-common-prefix "test" "test") "test"))
  (should (string-equal (wingman--string-common-prefix "" "hello") ""))
  (should (string-equal (wingman--string-common-prefix "hello" "") "")))

(ert-deftest test-wingman--buffer-lines ()
  "Test buffer line extraction function."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5")
    (should (equal (wingman--buffer-lines 1 3) '("line1" "line2" "line3")))
    (should (equal (wingman--buffer-lines 2 2) '("line2")))
    (should (equal (wingman--buffer-lines 1 1) '("line1")))
    (should (equal (wingman--buffer-lines 5 5) '("line5")))
    (should (null (wingman--buffer-lines 3 2)))))

(ert-deftest test-wingman--chunk-similarity ()
  "Test chunk similarity calculation."
  (let ((chunk1 (make-wingman--chunk :data '("line1" "line2" "line3")))
        (chunk2 (make-wingman--chunk :data '("line1" "line2" "line4")))
        (chunk3 (make-wingman--chunk :data '("line1" "line2" "line3"))))
    (should (= (wingman--chunk-similarity chunk1 chunk2) (/ (* 2.0 2) (+ 3 3)))) ; 2 common lines
    (should (= (wingman--chunk-similarity chunk1 chunk3) 1.0))
    (should (= (wingman--chunk-similarity chunk1 chunk1) 1.0))))

(ert-deftest test-wingman--should-be-disabled-p ()
  "Test disable predicate evaluation."
  (let ((wingman-disable-predicates '((lambda () t)))
        (wingman--default-disable-predicates nil))
    (should (wingman--should-be-disabled-p)))
  (let ((wingman-disable-predicates '((lambda () nil)))
        (wingman--default-disable-predicates nil))
    (should (not (wingman--should-be-disabled-p)))))

(ert-deftest test-wingman--collect-local-context ()
  "Test local context collection."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4\nline5")
    (goto-char (point-min))
    (forward-line 2) ; On line 3, "line3"
    (forward-char 3) ; After "lin"
    (let ((ctx (wingman--collect-local-context)))
      (should (equal (cdr (assoc 'prefix ctx)) "line1\nline2\n"))
      (should (equal (cdr (assoc 'middle ctx)) "lin"))
      (should (equal (cdr (assoc 'suffix ctx)) "e3\nline4\nline5\n"))
      (should (equal (cdr (assoc 'indent ctx)) 0))
      (should (equal (cdr (assoc 'line-prefix ctx)) "lin"))
      (should (equal (cdr (assoc 'line-suffix ctx)) "e3"))
      (should (equal (cdr (assoc 'line-full ctx)) "line3\n")))))

(ert-deftest test-wingman--random-chunk ()
  "Test random chunk selection."
  (let ((text '("line1" "line2" "line3" "line4" "line5" "line6" "line7" "line8"))
        (wingman-ring-chunk-size 64))
    (let ((chunk (wingman--random-chunk text)))
      (should (listp chunk))
      (should (<= (length chunk) (/ wingman-ring-chunk-size 2)))
      (should (> (length chunk) 0))
      (should (cl-every (lambda (line) (member line text)) chunk)))))

(ert-deftest test-wingman--cache-operations ()
  "Test cache get/put operations."
  (let ((wingman--cache (make-hash-table :test 'equal)))
    (wingman--cache-put "test-hash" "test-data")
    (should (string-equal (wingman--cache-get "test-hash") "test-data"))
    (should (null (wingman--cache-get "nonexistent")))))

(ert-deftest test-wingman--evict-similar-chunks ()
  "Test chunk eviction based on similarity."
  (let* ((chunk1 (make-wingman--chunk :data '("a" "b" "c")))
         (chunk2 (make-wingman--chunk :data '("a" "b" "d"))) ; high similarity to new
         (chunk3 (make-wingman--chunk :data '("x" "y" "z"))) ; low similarity
         (new-chunk (make-wingman--chunk :data '("a" "b" "e")))
         (wingman--ring-chunks (list chunk1 chunk2 chunk3)))
    (wingman--evict-similar-chunks new-chunk 0.5)
    ;; chunk1 and chunk2 should be evicted as they are > 0.5 similar to new-chunk
    (should (not (member chunk1 wingman--ring-chunks)))
    (should (not (member chunk2 wingman--ring-chunks)))
    ;; chunk3 should remain as its similarity is 0
    (should (member chunk3 wingman--ring-chunks))))

(ert-deftest test-wingman--safe-filename ()
  "Test privacy-safe filename generation."

  (let ((project-current-result nil))
    (cl-letf (((symbol-function 'project-current) (lambda () project-current-result)))
      (should (string-equal
               (wingman--safe-filename "/Users/foo/Documents/test.py")
               (abbreviate-file-name "/Users/foo/Documents/test.py")))

      (should (string-equal (wingman--safe-filename nil) "unnamed"))

      (should (string-equal (wingman--safe-filename "") "unnamed"))))

  (let* ((mock-project (list 'vc "/home/user/myproject/"))
         (project-root "/home/user/myproject/"))
    (cl-letf (((symbol-function 'project-current) (lambda () mock-project))
              ((symbol-function 'project-root) (lambda (_) project-root)))

      (should (string-equal
               (wingman--safe-filename "/home/user/myproject/src/main.py")
               "src/main.py"))

      (should (string-equal
               (wingman--safe-filename "/home/user/myproject/README.md")
               "README.md"))

      (should (string-equal
               (wingman--safe-filename "/home/user/other/file.py")
               "file.py"))

      (should (string-equal
               (wingman--safe-filename "/home/user/file.py")
               "file.py"))))

  (let* ((mock-project (list 'vc "/home/user/projects/myapp/"))
         (project-root "/home/user/projects/myapp/"))
    (cl-letf (((symbol-function 'project-current) (lambda () mock-project))
              ((symbol-function 'project-root) (lambda (_) project-root)))

      (should (string-equal
               (wingman--safe-filename "/home/user/projects/myapp/src/components/ui/button.tsx")
               "src/components/ui/button.tsx"))

      (should (string-equal
               (wingman--safe-filename "/home/user/myapp/test.py")
               "test.py")))))

(ert-deftest test-wingman--safe-filename-with-buffer-names ()
  "Test safe filename with buffer names (non-file buffers)."

  ;; Mock `project-current' to return nil to ensure these tests are not
  ;; affected by the environment. This correctly simulates the behaviour for
  ;; non-file-backed buffers.
  (cl-letf (((symbol-function 'project-current) (lambda () nil)))
    (should (string-equal (wingman--safe-filename "*scratch*") "*scratch*"))
    (should (string-equal (wingman--safe-filename "*Messages*") "*Messages*"))
    (should (string-equal (wingman--safe-filename "*temp-buffer*") "*temp-buffer*"))))

(ert-deftest test-wingman--build-emulated-fim-prompt-with-safe-filenames ()
  "Test that the emulated FIM prompt uses safe filenames."

  (let* ((mock-project (list 'vc "/home/user/myproject/"))
         (project-root "/home/user/myproject/")
         (major-mode 'python-mode)
         (ctx '((prefix . "def hello():\n    ")
                (middle . "print")
                (suffix . "\n    return 'world'")))
         (mock-extra-context
          (vector
           `((text . "import os\ndef util():\n    pass")
             (filename . "/home/user/myproject/src/utils.py")
             (time . 1234567890.0))
           `((text . "# Configuration\nDEBUG = True")
             (filename . "/home/user/other/config.py")
             (time . 1234567891.0)))))

    (cl-letf (((symbol-function 'project-current) (lambda () mock-project))
              ((symbol-function 'project-root) (lambda (_) project-root))
              ((symbol-function 'wingman--extra-context) (lambda (_) mock-extra-context))
              ((symbol-function 'wingman--lang-from-major-mode) (lambda (_) "python")))

      (let ((prompt (wingman--build-emulated-fim-prompt ctx)))
        ;; Should contain project-relative path for file within project
        (should (string-match-p "src/utils.py" prompt))
        ;; Should contain just filename for file outside project
        (should (string-match-p "config.py" prompt))
        ;; Should NOT contain full paths
        (should-not (string-match-p "/home/user/myproject/src/utils.py" prompt))
        (should-not (string-match-p "/home/user/other/config.py" prompt))))))

(ert-deftest test-wingman--build-emulated-fim-prompt-no-context ()
  "Test emulated FIM prompt generation with no extra context."

  (let* ((major-mode 'javascript-mode)
         (ctx '((prefix . "function test() {\n    ")
                (middle . "console")
                (suffix . "\n}"))))

    (cl-letf (((symbol-function 'wingman--extra-context) (lambda (_) []))
              ((symbol-function 'wingman--lang-from-major-mode) (lambda (_) "javascript")))

      (let ((prompt (wingman--build-emulated-fim-prompt ctx)))
        ;; Should not contain context section
        (should-not (string-match-p "Context from Recently Accessed Files" prompt))
        ;; Should contain the basic FIM structure
        (should (string-match-p "function test() {" prompt))
        (should (string-match-p "console<FIM_MARKER>" prompt))
        (should (string-match-p "javascript" prompt))))))

(ert-deftest test-wingman--build-emulated-fim-prompt-with-home-abbreviation ()
  "Test that files are properly abbreviated with ~ when no project context."

  (let* ((major-mode 'python-mode)
         (ctx '((prefix . "# Test\n")
                (middle . "import")
                (suffix . "\nprint('hello')")))
         (mock-extra-context
          (vector
           `((text . "def helper():\n    pass")
             (filename . "/Users/john/Documents/helper.py")
             (time . 1234567890.0)))))

    (cl-letf (((symbol-function 'project-current) (lambda () nil))
              ((symbol-function 'wingman--extra-context) (lambda (_) mock-extra-context))
              ((symbol-function 'wingman--lang-from-major-mode) (lambda (_) "python"))
              ((symbol-function 'abbreviate-file-name)
               (lambda (filename)
                 (if (string-prefix-p "/Users/john/" filename)
                     (concat "~/" (substring filename 12))
                   filename))))

      (let ((prompt (wingman--build-emulated-fim-prompt ctx)))
        ;; Should contain abbreviated path
        (should (string-match-p "~/Documents/helper.py" prompt))
        ;; Should NOT contain full path
        (should-not (string-match-p "/Users/john/Documents/helper.py" prompt))))))

(ert-deftest test-wingman--safe-filename-integration ()
  "Integration test for safe filename in context of actual chunk creation."

  (let* ((mock-project (list 'vc "/workspace/myapp/"))
         (project-root "/workspace/myapp/")
         (test-chunk (make-wingman--chunk
                      :data '("function test() {" "  return 42;" "}")
                      :filename "/workspace/myapp/src/main.js"
                      :timestamp 1234567890.0
                      :project-root project-root)))

    (cl-letf (((symbol-function 'project-current) (lambda () mock-project))
              ((symbol-function 'project-root) (lambda (_) project-root)))

      ;; Test that chunk filename gets properly sanitized
      (let ((safe-name (wingman--safe-filename (wingman--chunk-filename test-chunk))))
        (should (string-equal safe-name "src/main.js"))
        (should-not (string-match-p "/workspace/" safe-name))))))

(ert-deftest test-wingman-version-constant ()
  "Test that version constant is defined and valid."
  (should (boundp 'wingman-version))
  (should (stringp wingman-version))
  (should (string-match-p "^[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?$" wingman-version)))

(ert-deftest test-wingman-custom-variables ()
  "Test that custom variables are properly defined."
  (should (boundp 'wingman-n-prefix))
  (should (boundp 'wingman-n-suffix))
  (should (boundp 'wingman-ring-chunk-size))
  (should (boundp 'wingman-disable-predicates)))

(ert-deftest test-wingman--log ()
  "Test logging functionality."
  (let ((wingman-log-level 2)
        (wingman-log-buffer "*test-wingman-log*"))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create wingman-log-buffer) (erase-buffer))
          (wingman--log 1 "Test message %s" "arg")
          (with-current-buffer wingman-log-buffer
            (goto-char (point-min))
            (should (search-forward "Test message arg" nil t))))
      (when (get-buffer wingman-log-buffer)
        (kill-buffer wingman-log-buffer)))))


(provide 'wingman-tests)
;;; wingman-tests.el ends here
