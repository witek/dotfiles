;;; witek-defaults.el --- Witek's Defaults based on Crafted Defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(straight-use-package 'use-package)


(setq crafted-load-custom-file nil)

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq max-lisp-eval-depth 16000)
(setq max-specpdl-size 25000)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(setq cursor-in-non-selected-windows nil)

(setq x-stretch-cursor t)

(setq undo-limit 80000000)

(setq truncate-string-ellipsis "…")

(setq delete-by-moving-to-trash t)

(setq help-window-select t)

(setq initial-scratch-message "")


(setq recenter-positions '(5 top bottom))

(setq scroll-conservatively 101)

(customize-set-variable 'scroll-margin 35)

(setq scroll-preserve-screen-position t)

(setq sentence-end-double-space nil)

;; (setq uniquify-buffer-name-style 'complete)

(setq window-combination-resize t)

(global-display-fill-column-indicator-mode t)

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

(save-place-mode 1)

(setq create-lockfiles nil)

;; *** Auto Save

(let ((witek-auto-save-directory (concat crafted-config-var-directory "witek-auto-save/")))
  (setq backup-directory-alist
        `((".*" . ,witek-auto-save-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,witek-auto-save-directory t))))

(setq auto-save-default t)


;; *** Let's be Evil



;; ** UI


;; *** windows


;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

;; *** consult

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)





;; *** my custom keymaps

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-c C-c") 'execute-extended-command)
(global-set-key (kbd "C-c <SPC>") 'save-buffer)

(global-set-key (kbd "C-c f q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-c f Q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c f e") 'eval-expression)
(global-set-key (kbd "C-c f f") 'make-frame)

(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c f S") 'save-some-buffers)
(global-set-key (kbd "C-c f f") 'find-file)

(global-set-key (kbd "C-c t s") 'smartparens-mode)
(global-set-key (kbd "C-c t S") 'smartparens-strict-mode)

(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)



(global-set-key (kbd "C-c f r") 'consult-recent-file)
(global-set-key (kbd "C-c p /") 'consult-git-grep)

;; *** which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))


;; * general - Leader Keybindings
;; general.el for prefixed key bindings
;; https://github.com/noctuid/general.el
(use-package general
  :straight t)



(general-create-definer my-leader-def
  :states (list 'normal 'visual)
  :prefix "SPC")


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

(my-local-leader-def
  :keymaps (list 'with-editor-mode-map)
  "," 'with-editor-finish
  "q" 'with-editor-cancel
  )



;; * project


(customize-set-variable 'project-vc-merge-submodules t)

;; project-find-functions
;; (project-try-vc "/p/clj-crafted/kunagi-utils")


;; ** magit - git Version Control

(use-package magit
  :straight t
  :bind (("C-c g s" . 'magit-status)))

;; ** Editing


;; Default to an indentation size of 2 spaces
(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

(customize-set-variable 'fill-column 80)

(use-package clean-kill-ring
  :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
  :config
  (clean-kill-ring-mode 1))


;; (crafted-package-install-package 'visual-fill-column)
;; (require 'visual-fill-column)
;; (custom-set-variables
;;  '(global-visual-fill-column-mode t)
;;  '(visual-fill-column-width 80))

;; *** Commenting / Uncommenting


;; *** smartparens - Structural Editing
;; [[https://github.com/Fuco1/smartparens]]
;; [[https://github.com/expez/evil-smartparens]]



;; (smartparens-global-strict-mode)


;; (general-define-key
;;  :states '(normal visual)
;;  "c-l" 'evil-cp->
;;  "c-h" 'evil-cp-<
;;  )

;; *** symex - Structural Editing
;; [[https://github.com/countvajhula/symex.el]]

;; (crafted-package-install-package 'symex)
;; (symex-initialize)

;; *** Keybindings

(my-local-leader-def
  :keymaps (list 'emacs-lisp-mode-map 'clojure-mode-map)
  "(" 'sp-wrap-round
  "[" 'sp-wrap-square
  "{" 'sp-wrap-curly
  ;; "=" 'indent-sexp
  "=" 'sp-indent-defun
  )


;; * Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

(my-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e v" 'eval-last-sexp
  "e b" 'eval-buffer)



;; * Export witek-defaults
(provide 'witek-defaults)
