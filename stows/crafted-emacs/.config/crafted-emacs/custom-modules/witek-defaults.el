;; witek-defaults.el --- Witek's Defaults based on Crafted Defaults  -*- lexical-binding: t; -*-

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

;; Default to an indentation size of 2 spaces
(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

(customize-set-variable 'fill-column 80)

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


(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)


(customize-set-variable 'project-vc-merge-submodules t)

;; *** my custom keymaps

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-c C-c") 'execute-extended-command)
(global-set-key (kbd "C-c <RET>") 'save-buffer)

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

(global-set-key (kbd "C-c x") ctl-x-map)

;; * Export witek-defaults
(provide 'witek-defaults)
