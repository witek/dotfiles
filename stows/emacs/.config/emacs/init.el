;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>


;;; Setup Emacs Internals

(setq load-prefer-newer t)
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq max-lisp-eval-depth 16000)
(setq max-specpdl-size 25000)


;;; My custom modules

(let ((custom-modules (expand-file-name "custom-modules" user-emacs-directory)))
  (when (file-directory-p custom-modules)
    (message "adding custom-modules to load-path: %s" custom-modules)
    (add-to-list 'load-path custom-modules)))


;;; Setup Custom

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))


;;; Install packages

(setq use-package-compute-statistics t)


(add-to-list 'package-selected-packages 'all-the-icons)
(add-to-list 'package-selected-packages 'elisp-demos)
(add-to-list 'package-selected-packages 'helpful)
(add-to-list 'package-selected-packages 'aggressive-indent)
(add-to-list 'package-selected-packages 'vertico)
(add-to-list 'package-selected-packages 'cape)
(add-to-list 'package-selected-packages 'consult)
(add-to-list 'package-selected-packages 'corfu)
(add-to-list 'package-selected-packages 'corfu-terminal)
(add-to-list 'package-selected-packages 'nerd-icons-corfu)
(add-to-list 'package-selected-packages 'embark)
(add-to-list 'package-selected-packages 'embark-consult)
(add-to-list 'package-selected-packages 'marginalia)
(add-to-list 'package-selected-packages 'orderless)
(add-to-list 'package-selected-packages 'ef-themes)
(add-to-list 'package-selected-packages 'spacious-padding)
(add-to-list 'package-selected-packages 'pulsar)
(add-to-list 'package-selected-packages 'yasnippet)
(add-to-list 'package-selected-packages 'doom-modeline)
(add-to-list 'package-selected-packages 'doom-themes)
(add-to-list 'package-selected-packages 'meow)
(add-to-list 'package-selected-packages 'outline)
(add-to-list 'package-selected-packages 'outline-minor-faces)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'hi-lock)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'dirvish)
(add-to-list 'package-selected-packages 'smartparens)
(add-to-list 'package-selected-packages 'phi-search)
(add-to-list 'package-selected-packages 'paren-face)
(add-to-list 'package-selected-packages 'popper)
(add-to-list 'package-selected-packages 'avy)
(add-to-list 'package-selected-packages 'wgrep)
(add-to-list 'package-selected-packages 'treemacs)
(add-to-list 'package-selected-packages 'adoc-mode)
(add-to-list 'package-selected-packages 'hl-todo)
(add-to-list 'package-selected-packages 'cider)
(add-to-list 'package-selected-packages 'clj-refactor)
(add-to-list 'package-selected-packages 'clojure-mode)
(add-to-list 'package-selected-packages 'flycheck-clojure)
(add-to-list 'package-selected-packages 'figlet)
(add-to-list 'package-selected-packages 'gptel)
;; (add-to-list 'package-selected-packages 'gptel-autocomplete)
(add-to-list 'package-selected-packages 'elysium)
(add-to-list 'package-selected-packages 'lorem-ipsum)
;; (add-to-list 'package-selected-packages 'consult-omni)
(add-to-list 'package-selected-packages 'go-translate)
(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'lsp-ui)
(add-to-list 'package-selected-packages 'lsp-treemacs)
(add-to-list 'package-selected-packages 'consult-lsp)
(add-to-list 'package-selected-packages 'ligature)
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'org)
(add-to-list 'package-selected-packages 'org-modern)
(add-to-list 'package-selected-packages 'org-appear)
(add-to-list 'package-selected-packages 'denote)
(add-to-list 'package-selected-packages 'denote-journal)
(add-to-list 'package-selected-packages 'outshine)
(add-to-list 'package-selected-packages 'linum-relative)
(when (executable-find "latex")
  (add-to-list 'package-selected-packages 'auctex))
(when (and (executable-find "latex")
           (executable-find "latexmk"))
  (add-to-list 'package-selected-packages 'auctex-latexmk))
(add-to-list 'package-selected-packages 'binky)
(add-to-list 'package-selected-packages 'string-edit-at-point)
(add-to-list 'package-selected-packages 'substitute)
(add-to-list 'package-selected-packages 'mu4easy)
(add-to-list 'package-selected-packages 'edit-server)
(add-to-list 'package-selected-packages 'gmail-message-mode)
(add-to-list 'package-selected-packages 'chatgpt-shell)
;; (add-to-list 'package-selected-packages 'dall-e-shell)
(add-to-list 'package-selected-packages 'ob-chatgpt-shell)
(add-to-list 'package-selected-packages 'blamer)
(add-to-list 'package-selected-packages 'aidermacs)
(add-to-list 'package-selected-packages 'multifiles)
;; (add-to-list 'package-selected-packages 'app-launcher)

;; Install
(package-install-selected-packages :noconfirm)


;;; C-c custom keys (used as leader)

(defun my/set-custom-key (kbd-string command-symbol)
  (global-set-key (kbd (concat "C-c " kbd-string)) command-symbol))

;;; my custom context key-map

(defvar witek-context-key-map (make-sparse-keymap) "My Context Keymap")
(defalias 'witek-context-key-map witek-context-key-map)

(defun my/set-context-key (kbd-string command-symbol)
  (define-key witek-context-key-map (kbd kbd-string) command-symbol))

(defun my/activate-context-key-map ()
  "Set `witek-context-key-map' as the current transient map. Also show which-key."
  (interactive)
  (set-transient-map witek-context-key-map))

;;; My Stuff

(require 'my-defaults)
(require 'my-commands)
(require 'my-meow)
(require 'my-theme)
(require 'my-keys)
(require 'my-basics)
(require 'my-org)
(require 'my-extras)

(require 'my-ai)

;; (require 'my-eglot)
(require 'my-lsp)

;; (require 'my-email)











