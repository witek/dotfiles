;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;(straight-use-package 'use-package)


;; (setq-default left-fringe-width  16)
;; (setq-default right-fringe-width  16)
(set-frame-parameter nil 'internal-border-width 8)

;; (crafted-package-install-package 'spacegray-theme)
;; (use-package spacegray-theme
;;   )

;;; doom-themes

(crafted-package-install-package 'doom-themes)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  (consult-theme 'doom-one)
  ;; (load-theme 'doom-one t)


  )

;;; fira-code-mode

(crafted-package-install-package 'fira-code-mode)
(use-package fira-code-mode
  :config
  (global-fira-code-mode)

  (cond

   ((> (x-display-pixel-height) 1600)
    (customize-set-variable 'crafted-ui-default-font
                            '(:font "Fira Code" :height 110)))

   (:else
    (customize-set-variable 'crafted-ui-default-font
                            '(:font "Fira Code" :height 110))
    ))
  )

;;; doom-modeline

(crafted-package-install-package 'doom-modeline)
(use-package doom-modeline
  :init
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 35)
  ;; (setq doom-modeline-hud t)
  ;; (setq doom-modeline-bar-width 20)
  (setq doom-modeline-window-width-limit 20)
  ;; (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info t)
  (doom-modeline-mode 1)
  )

;;; outline

(use-package outline
  :config
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'clojure-mode-hook #'outline-minor-mode))

;;; outline-minor-faces

(crafted-package-install-package 'outline-minor-faces)
(use-package outline-minor-faces
  :after outline

  :config
  (add-hook 'outline-minor-mode-hook
            #'outline-minor-faces-mode))

;;; custom faces

(set-face-attribute 'mode-line nil :inherit 'variable-pitch)

(custom-set-faces
 ;; '(org-level-1 ((t (:inherit default :weight bold :foreground "gray80" :font "Source Sans Pro" :height 1.75))))
 '(outline-minor-1 ((t (:inherit default :foreground "gray60" :font "Rubik Dirt" :height 2.0))))
 ;;
 )

;;; provide

(provide 'witek-theme)
