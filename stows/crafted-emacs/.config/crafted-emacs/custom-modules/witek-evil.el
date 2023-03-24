;;; witek-defaults.el --- Witek's Defaults based on Crafted Defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(straight-use-package 'use-package)

(require 'crafted-evil)

(setq-default evil-shift-width tab-width)

(customize-set-variable 'evil-want-C-u-delete nil)
(customize-set-variable 'evil-want-C-u-scroll t)
(customize-set-variable 'evil-want-C-i-jump t)
(customize-set-variable 'evil-want-C-d-scroll t)
(customize-set-variable 'evil-want-Y-yank-to-eol nil) ; does not work
(customize-set-variable 'evil-regexp-search nil) ; does not work
(customize-set-variable 'evil-cross-lines t)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(use-package general
  :straight t
  :config
  (general-evil-setup))


(general-create-definer my-leader-def
  :states (list 'normal 'visual)
  :prefix "SPC")


(my-leader-def
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w h" 'evil-window-left
  "w l" 'evil-window-right
  "w /" 'evil-window-vsplit
  "w -" 'evil-window-split
  )

(general-define-key
 :states '(normal visual)
 ";" 'evilnc-comment-or-uncomment-lines)



(my-leader-def
  "SPC" 'execute-extended-command
  "/" 'consult-git-grep

;;  "e" 'witek-e-key-map
;;  "f" 'witek-f-key-map
;;  "t" 'witek-f-key-map


  "b b" 'consult-buffer
  "b d" 'kill-current-buffer

  ;; "f s" 'save-buffer
  ;; "f S" 'save-some-buffers
  ;; "f f" 'find-file
  ;; "f r" 'consult-recent-file

  "w d" 'delete-window
  "w w" 'other-window

  "d k" 'describe-key
  "d c" 'describe-command
  "d v" 'describe-variable
  "d f" 'describe-function
  "d m" 'describe-mode
  "d K" 'describe-keymap


  )

(general-define-key
 :keymaps 'vertico-map
 :prefix "C-,"
 "e" 'embark-export
 "a" 'embark-act)

;; ** Local Leader

(general-create-definer my-local-leader-def
  :states (list 'normal 'visual)
  :prefix ",")


(use-package evil-smartparens
  :straight t)

(require 'smartparens-config)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; *** cleverparens
;; https://github.com/luxbock/evil-cleverparens
(use-package evil-cleverparens
  :straight t
  :init
  (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)
  :config
  (general-define-key
   :keymaps 'evil-cleverparens-mode-map
   :states 'normal
   "M-l" 'evil-cp->
   "M-h" 'evil-cp-<))

(require 'evil-cleverparens-text-objects)

(my-local-leader-def
  :keymaps (list 'emacs-lisp-mode-map 'clojure-mode-map)
  ", i" 'evil-cp-insert-at-beginning-of-form
  ", a" 'evil-cp-insert-at-end-of-form
  )

(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'clojure-mode-hook #'evil-smartparens-mode)


(my-local-leader-def
  :keymaps (list 'with-editor-mode-map)
  "," 'with-editor-finish
  "q" 'with-editor-cancel
  )



(my-local-leader-def
  :keymaps (list 'emacs-lisp-mode-map 'clojure-mode-map)
  "(" 'sp-wrap-round
  "[" 'sp-wrap-square
  "{" 'sp-wrap-curly
  ;; "=" 'indent-sexp
  "=" 'sp-indent-defun
  )


;; * Emacs Lisp


(my-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e v" 'eval-last-sexp
  "e b" 'eval-buffer)




;;
;; ----
;;
(provide 'witek-evil)
