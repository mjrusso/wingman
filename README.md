#  wingman.el - LLM-assisted text completion

Wingman brings GitHub Copilot-style inline completions to Emacs, offering two powerful modes: fast, automatic completions via a local (or remote) [llama.cpp](https://github.com/ggml-org/llama.cpp) server, in addition to manual completions through any backend supported by [gptel](https://github.com/karthink/gptel).

_This package is, primarily, a direct port of the excellent [llama.vim plugin](https://github.com/ggml-org/llama.vim) (see [technical design details](https://github.com/ggml-org/llama.cpp/pull/9787) for more background on how this works)._

![logo](./assets/logo.png)

### Features

* **Inline "Ghost Text" Completions:** Suggestions appear directly in your buffer as you type.
* **Dual Completion Modes:**
  - **Native FIM:** Fast, efficient completions via `llama.cpp`'s specialized `/infill` endpoint for "fill-in-the-middle" (FIM) models. By default, these completions appear automatically as you type.
  - **Emulated FIM:** Access a broader range of models via `gptel`. These completions must always be manually triggered.
* **Local First:** No code leaves your machine when using a self-hosted `llama.cpp` server with the _Native FIM_ completion model.
* **Asynchronous by Default:** Never blocks your editing while waiting for a completion.
* **Response Caching:** Repeated requests for the same context are answered instantly from an in-memory cache.
* **Project-Aware Context:** Uses a ring buffer of text chunks from recently used files (scoped to the [current project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)) to provide more relevant suggestions.

## Prerequisites

This package requires a running [llama.cpp](https://github.com/ggml-org/llama.cpp) server instance, accessible at `wingman-llama-endpoint`.

Additionally, to use the _Emulated FIM_ completion mode, you must configure [gptel](https://github.com/karthink/gptel) accordingly for each LLM provider you intend to use.

## Installation

### With `use-package` and `straight.el`

```emacs-lisp
(use-package wingman
  :straight (:type git :host github :repo "mjrusso/wingman")
```

Note that this package depends on [transient](https://github.com/magit/transient) and [gptel]((https://github.com/karthink/gptel)), which will be installed automatically if necessary.

### Manual Installation

Clone this repository:

```bash
git clone https://github.com/mjrusso/wingman.git ~/.emacs.d/lisp/wingman
```

Add then add the directory to your Emacs `load-path`:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp/wingman")
(require 'wingman)
```

Note that dependencies ([transient](https://github.com/magit/transient) and [gptel]((https://github.com/karthink/gptel)) must also be manually installed in this case.

## Configuration

The behaviour of Wingman can be customized through its customization group (`M-x customize-group RET wingman RET`), or by setting variables in your `init.el`.

A minimal configuration:

```emacs-lisp
(use-package wingman
  :straight (:type git :host github :repo "mjrusso/wingman")

  ;; Enable wingman-mode in all programming modes
  :hook (prog-mode . wingman-mode))
```

An example of a more advanced configuration:

```emacs-lisp
(use-package wingman
  :straight (:type git :host github :repo "mjrusso/wingman")
  :ensure t
  :defer t

  :init

  (setq wingman-prefix-key (kbd "C-c w"))

  :hook (prog-mode . wingman-mode)

  :config

  (setq wingman-log-level 4)
  (setq wingman-ring-n-chunks 16)

  ;; default llama.cpp server port is 8012
  (setq wingman-llama-endpoint "http://127.0.0.1:8080/infill")

  ;; assumes use of Modus Themes; substitute with preferred color scheme
  (set-face-attribute 'wingman-overlay-face nil
                      :foreground  (modus-themes-get-color-value 'red-warmer)
                      :background  (modus-themes-get-color-value 'bg-inactive))

  ;; don't provide completions in files that typically contain secrets
  (add-to-list 'wingman-disable-predicates
               (lambda ()
                 (or (derived-mode-p 'envrc-file-mode)
                     (derived-mode-p 'direnv-envrc-mode)
                     (when buffer-file-name
                       (let ((fname (file-name-nondirectory buffer-file-name)))
                         (or (string-equal ".env" fname)
                             (string-equal ".envrc" fname)
                             (string-prefix-p ".localrc" fname)))))))

  :bind
  (:map wingman-mode-prefix-map
   ("TAB" . wingman-fim)              ; Request Native FIM
   ("S-TAB" . wingman-fim-emulated)   ; Request Emulated FIM
   ("d" . wingman-fim-debug))

   :map wingman-mode-completion-transient-map
   ("TAB" . wingman-accept-full)
   ("S-TAB" . wingman-accept-line)
   ("M-S-TAB" . wingman-accept-word)))
```

## Usage

1. **Enable the mode:** `wingman-mode` is a buffer-local minor mode. You can enable it with `M-x wingman-mode`, or globally in all applicable buffers with `global-wingman-mode`. (Alternatively, you may enable `wingman-mode` automatically via the hook as shown in the configuration examples above.)

2. **Get Completions:**
   * **Native FIM (`wingman-fim`):**
     * If `wingman-auto-fim` is `t` (the default), completions will appear automatically as you type using the configured `llama.cpp` server.
     * To manually request a completion from the configured `llama.cpp` server, use `M-x wingman-fim` (bound to `C-c w TAB` in the example config).
   * **Emulated FIM (`wingman-fim-emulated`):****
     * This is always triggered manually. Use `M-x wingman-fim-emulated` (bound to `C-c w S-TAB` in the example config).
     * Once triggered, a `transient` menu will appear, letting you choose from your configured `gptel` backends and models. Select one to send the request.

3. **Accept a Completion:**
   * **Full:** Press the "accept full" key (default: `<tab>`) to insert the entire suggestion.
   * **Line:** Press the "accept line" key (default: `S-TAB`) to insert only the first line of the suggestion.
   * **Word:** Press the "accept word" key (default: `M-S-TAB`) to insert only the first word.

4. **Dismiss a Completion:**
   * Keep typing, or
   * Move the cursor, or
   * Press the manual trigger key again.

## Appendix

### llama.cpp setup

#### Mac OS

```bash
brew install llama.cpp
```

#### Windows

```bash
winget install llama.cpp
```

#### Other OS

Install from your preferred package manager, build from source, or use the [latest binaries](https://github.com/ggml-org/llama.cpp/releases).

### llama.cpp settings

[Recommended settings](https://github.com/ggml-org/llama.vim/blob/master/README.md#llamacpp-settings), depending on the amount of available VRAM:

- More than 16GB VRAM:

  ```bash
  llama-server --fim-qwen-7b-default
  ```

- Less than 16GB VRAM:

  ```bash
  llama-server --fim-qwen-3b-default
  ```

- Less than 8GB VRAM:

  ```bash
  llama-server --fim-qwen-1.5b-default
  ```

Note that a [FIM ("fill-in-the-middle")-compatible model](https://huggingface.co/collections/ggml-org/llamavim-6720fece33898ac10544ecf9) is required.

### gptel setup

To use _Emulated FIM_ mode, configure `gptel` backends as per [the setup instructions](https://github.com/karthink/gptel?tab=readme-ov-file#setup).

## Acknowledgements

This project is a direct port of [llama.vim](https://github.com/ggml-org/llama.vim) to Emacs.

Also see: [copilot.el](https://github.com/copilot-emacs/copilot.el) and [emacs-copilot](https://github.com/jart/emacs-copilot) for alternative approaches and inspiration.

## License

This project is licensed under the GPL-3.0-or-later License. See the `LICENSE` file for details.
