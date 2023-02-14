;;; witek-defaults.el --- Witek's Defaults based on Crafted Defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(straight-use-package 'use-package)

(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-evil)
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-editing)     ; Whitespace trimming, auto parens etc.
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-speedbar)    ; built-in file-tree
(require 'crafted-lisp)
(require 'crafted-project)
(require 'crafted-compile)     ; Set up automatic compilation for some emacs-lisp files

(setq crafted-load-custom-file nil)

;; *** TODO Start Emacs server (for emacsclient)
;;(server-start)

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(setq read-process-output-max (* 1024 1024))

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


;; Use C-h as usual
(customize-set-variable 'evil-want-C-u-delete nil)
(customize-set-variable 'evil-want-C-u-scroll t)
(customize-set-variable 'evil-want-C-i-jump t)
(customize-set-variable 'evil-want-C-d-scroll t)
(customize-set-variable 'evil-want-Y-yank-to-eol nil) ; does not work
(customize-set-variable 'evil-regexp-search nil) ; does not work
(customize-set-variable 'evil-cross-lines t)
;; alskdjf löaskdjf ölaksjd flökasjdflöksj dflkjsd alskjdf laskdjf aösldk aslködjf aslkdjf alskdjf

;; ** UI


;; (setq-default left-fringe-width  16)
;; (setq-default right-fringe-width  16)
(set-frame-parameter nil 'internal-border-width 8)

;; *** windows

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

;; *** consult

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)



;; *** which-key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; *** Theme
;; also loaded in early-config.el
(use-package spacegray-theme
  :straight t)
(use-package doom-themes
  :straight t
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

;; *** Font

(use-package fira-code-mode
  :straight t
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


;; * general - Leader Keybindings
;; general.el for prefixed key bindings
;; https://github.com/noctuid/general.el
(use-package general
  :straight t
  :config
  (general-evil-setup))


;; ** Leader

(general-create-definer my-leader-def
  :states (list 'normal 'visual)
  :prefix "SPC")

(my-leader-def
  "SPC" 'execute-extended-command
  "/" 'consult-git-grep

  "e q" 'save-buffers-kill-terminal
  "e Q" 'save-buffers-kill-emacs
  "e e" 'eval-expression
  "e f" 'make-frame

  "q q" 'save-buffers-kill-terminal
  "q Q" 'save-buffers-kill-emacs

  "b b" 'consult-buffer
  "b d" 'kill-current-buffer

  "f s" 'save-buffer
  "f S" 'save-some-buffers
  "f f" 'find-file
  "f r" 'consult-recent-file

  "w d" 'delete-window
  "w w" 'other-window
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w h" 'evil-window-left
  "w l" 'evil-window-right
  "w /" 'evil-window-vsplit
  "w -" 'evil-window-split

  "d k" 'describe-key
  "d c" 'describe-command
  "d v" 'describe-variable
  "d f" 'describe-function
  "d m" 'describe-mode
  "d K" 'describe-keymap

  "t s" 'smartparens-mode
  "t S" 'smartparens-strict-mode

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

;; * treemacs

(use-package treemacs
  :straight t

  :init
  (setq treemacs-space-between-root-nodes nil)

  :config
  (my-local-leader-def
    :keymaps (list 'treemacs-mode-map)
    "," 'treemacs-mark-or-unmark-path-at-point
    "x" 'treemacs-delete-file
    "m" 'treemacs-move-file
    "c" 'treemacs-copy-file
    "r" 'treemacs-rename-file
    )
  )

;; * Restart Emacs

(use-package restart-emacs
  :straight t
  :config
  (my-leader-def
    "q r" 'restart-emacs
    "e r" 'restart-emacs))


;; * project


(customize-set-variable 'project-vc-merge-submodules t)

;; project-find-functions
;; (project-try-vc "/p/clj-crafted/kunagi-utils")

(my-leader-def
  "p p" 'project-switch-project
  "p f" 'project-find-file
  "p /" 'consult-git-grep)

;; ** magit - git Version Control

(use-package magit
  :straight t
  :config
  (my-leader-def
    "g s" 'magit-status))

;; ** Editing


;; Default to an indentation size of 2 spaces
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

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

(general-define-key
 :states '(normal visual)
 ";" 'evilnc-comment-or-uncomment-lines)

;; *** smartparens - Structural Editing
;; [[https://github.com/Fuco1/smartparens]]
;; [[https://github.com/expez/evil-smartparens]]

(use-package evil-smartparens
  :straight t)

(require 'smartparens-config)

;; (smartparens-global-strict-mode)

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
  ", i" 'evil-cp-insert-at-beginning-of-form
  ", a" 'evil-cp-insert-at-end-of-form
  )



;; * Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)

(my-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e v" 'eval-last-sexp
  "e b" 'eval-buffer)



;; * Export witek-defaults
(provide 'witek-defaults)
