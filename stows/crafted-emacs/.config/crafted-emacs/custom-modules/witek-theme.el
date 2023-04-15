;;; witek-theme.el --- Theme Configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;(straight-use-package 'use-package)


;; (setq-default left-fringe-width  16)
;; (setq-default right-fringe-width  16)
(set-frame-parameter nil 'internal-border-width 8)

(crafted-package-install-package 'spacegray-theme)
(use-package spacegray-theme
  ;; :straight t
  )

(crafted-package-install-package 'doom-themes)
(use-package doom-themes
  ;; :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (load-theme 'spacegray t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


(crafted-package-install-package 'fira-code-mode)
(use-package fira-code-mode
  ;; :straight t
  :config
  (global-fira-code-mode))

(cond

 ((> (x-display-pixel-height) 1600)
  (customize-set-variable 'crafted-ui-default-font
                          '(:font "Fira Code" :height 110)))

 (:else
  (customize-set-variable 'crafted-ui-default-font
                          '(:font "Fira Code" :height 110))
  ))

(provide 'witek-theme)
