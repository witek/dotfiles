;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; Personal Information

(setq user-full-name "Witoslaw Koczewski")
(setq user-mail-address "wi@koczewski.de")

;;; Performance Optimizations

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq max-lisp-eval-depth 16000)
(setq max-specpdl-size 25000)

;;; Sensible defaults

(setq ad-redefinition-action 'accept)

(setq cursor-in-non-selected-windows nil)

(setq x-stretch-cursor t)

(setq undo-limit 80000000)

(setq truncate-string-ellipsis "…")

(setq help-window-select t)

(setq initial-scratch-message "")

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; ** automatic saving and backups

(let ((my/auto-save-directory (concat crafted-config-var-directory "witek-auto-save/")))
  (setq backup-directory-alist
        `((".*" . ,my/auto-save-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,my/auto-save-directory t))))

;; Activate auto saving in every buffer
(setq auto-save-default t)

(save-place-mode 1)

(setq create-lockfiles nil)

(setq delete-by-moving-to-trash t)

;;; buffers & editing

(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(setq next-line-add-newlines t)

(global-display-fill-column-indicator-mode t)

(customize-set-variable 'fill-column 80)

(setq recenter-positions '(5 top bottom))

(setq scroll-conservatively 101)

(customize-set-variable 'scroll-margin 35)

(setq scroll-preserve-screen-position t)

;;; navigation

;; just use identifier at point
(setq xref-prompt-for-identifier nil)


;;; files and directories

(setq vc-follow-symlinks t)

(customize-set-variable 'project-vc-merge-submodules t)

;;; external tools

(setq browse-url-browser-function 'browse-url-chrome)

;;; provide
(provide 'witek-defaults)
