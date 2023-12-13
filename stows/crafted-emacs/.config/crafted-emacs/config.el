;;; crafted emacs

(setq crafted-load-custom-file nil)
(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-editing)     ; Whitespace trimming, auto parens etc.
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-lisp)
(require 'crafted-project)
(require 'crafted-compile)     ; Set up automatic compilation for some emacs-lisp files

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

;;; my custom sub-configs

(require 'witek-defaults)
(require 'witek-keys)
(require 'witek-commands)


;; (require 'witek-evil)
(require 'witek-meow)

;; (require 'witek-eglot)
(require 'witek-lsp)

(require 'witek-extras)

(require 'witek-org)
(require 'witek-theme)

;;; server

(server-start)
