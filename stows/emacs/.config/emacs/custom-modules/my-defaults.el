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

(setq undo-limit 1600000)

(setq truncate-string-ellipsis "…")

(setq help-window-select t)

(setq initial-scratch-message "")

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; ** automatic saving and backups

(let ((my/auto-save-directory (expand-file-name "auto-save/" user-emacs-directory)))
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

(setq next-line-add-newlines nil)

(setq-default truncate-lines nil)
(setq truncate-lines nil)


;; (global-display-fill-column-indicator-mode t)

(customize-set-variable 'fill-column 80)

(setq recenter-positions '(5 top bottom))

(setq scroll-conservatively 101)

(customize-set-variable 'scroll-margin 10)

(setq scroll-preserve-screen-position t)

(setq global-visual-line-mode t)

;;; navigation

;; just use identifier at point
(setq xref-prompt-for-identifier nil)

(setq xref-auto-jump-to-first-xref 'show)

;;; files and directories

(setq vc-follow-symlinks t)

(customize-set-variable 'project-vc-merge-submodules t)

;;; external tools

;; (setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-browser-function 'browse-url-chrome)

;;; provide
(provide 'my-defaults)




