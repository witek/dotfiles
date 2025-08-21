;;; early-init.el --- Tangeled from emacs-config.org  -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(setq read-process-output-max (* 1024 1024))
(setq max-lisp-eval-depth 16000)
(setq max-specpdl-size 25000)

;; Temporarily increase the garbage collection threshold.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar my/--file-name-handler-alist file-name-handler-alist)
(defvar my/--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 16)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/--file-name-handler-alist
                  vc-handled-backends my/--vc-handled-backends)))

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'emacs-startup-hook
          (lambda ()
            (tool-bar-mode -1)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(push '(fullscreen . maximized) default-frame-alist)

(load-theme 'modus-vivendi-tinted)
