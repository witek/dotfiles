;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; eldoc

(require 'eldoc)

;;; helpful

(use-package helpful
  :config
  (my/set-custom-key "d v" 'helpful-variable)
  (my/set-custom-key "d k" 'helpful-key)
  (my/set-custom-key "d f" 'helpful-function)
  )

;;; aggressive-indent-mode

;; (when (locate-library "aggressive-indent")
  ;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;;; linum-relative
;; https://github.com/coldnew/linum-relative

;; (require 'linum-relative)
;; (setq linum-relative-backend 'display-line-numbers-mode)
;; (linum-relative-global-mode 1)

;;; which-key
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-show-transient-maps t)
  (setq which-key-idle-delay 0.3)
  (which-key-mode)
  )

;;; hi-lock
;; https://www.emacswiki.org/emacs/HiLock

(use-package hi-lock
  :bind
  (:map witek-context-key-map
        ("h s"        . 'highlight-symbol-at-point)
        ("h <escape>" . 'my/unhighlight-all-in-buffer))
  
  :config

  (defun my/unhighlight-all-in-buffer ()
    "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
    (interactive)
    (unhighlight-regexp t))
   
  )

;;; clean-kill-ring

;; (crafted-package-install-package 'clean-kill-ring)
;; (use-package clean-kill-ring
;;   ;; :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
;;   :config
;;   (clean-kill-ring-mode 1))

;;; yashippet
;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1))

;;; dirvish

(use-package dirvish
  :defer t
  :init
  (dirvish-override-dired-mode))

;;; treemacs

;; (use-package treemacs
;;   :init
;;   (setq treemacs-space-between-root-nodes nil)

;;   :config
;;   )

;;; phi-search
;; https://github.com/zk-phi/phi-search

;; (use-package phi-search
;;   :bind
;;   (:map global-map
;;         ("C-s" . 'phi-search)
;;         ("C-r" . 'phi-search-backward))
;;   )

;;; paren-face
;; https://github.com/tarsius/paren-face

(use-package paren-face
  :init
  (global-paren-face-mode 1)
  :config
  (set-face-attribute 'parenthesis nil :foreground "#5f5f5f")
  )

;;; popper
;; https://github.com/karthink/popper

(use-package popper

  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode))

  ;; (setq popper-window-height 20)
  
  (popper-mode 1)
  (popper-echo-mode 1)
  
  :bind
  ;; (setq popper-display-function #'display-buffer-in-child-frame)
  ("C-c t p" . popper-toggle))

;;; wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep

(use-package wgrep
  :after embark-consult
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :config
  ;; (my-local-leader-def
  ;;   :keymaps '(grep-mode-map wgrep-mode-map)
  ;;   "e" 'wgrep-change-to-wgrep-mode
  ;;   "," 'wgrep-finish-edit
  ;;   "q" 'wgrep-exit)
  )

;;; avy
;; https://github.com/abo-abo/avy

;; (use-package avy
;;   :defer t
;;   :bind ("C-j" . avy-goto-char-timer))

;;; adoc-mode

(use-package adoc-mode
  :defer t
   )


;;; clojure-mode

(use-package clojure-mode
  :defer t
  :config
  (setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically t
   )

  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  
  ;; (with-eval-after-load "flycheck"
    ;; (flycheck-clojure-setup))
  
  )

;;; cider

(use-package cider
  :defer t
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp)
  (setq cider-font-lock-reader-conditionals nil)
  (setq cider-auto-inspect-after-eval t)
  (setq cider-save-file-on-load t)
  (setq cider-dynamic-indentation nil)

  (define-key witek-context-key-map (kbd "e b") 'cider-eval-buffer)
  (define-key witek-context-key-map (kbd "e l") 'cider-eval-last-sexp)
  (define-key witek-context-key-map (kbd "e s") 'cider-eval-region)

  )

;;; clj-refactor

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))

;;; blamer

(use-package blamer
  ;; :bind (("s-i" . blamer-show-commit-info)
  ;;        ("C-c i" . blamer-show-posframe-commit-info))
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  ;; (blamer-face ((t :foreground "#7a88cf"
  ;;                   :background nil
  ;;                   :height 140
  ;;                   :italic t)))
  :config
  (global-blamer-mode 1))

;;; figlet
;; https://www.emacswiki.org/emacs/Figlet

(use-package figlet
  :defer t)


;;; lorem-ipsum

(use-package lorem-ipsum
  :defer t)

;;; go-translate

;; (use-package go-translate
;;   :defer t
;;   :init
;;   (require 'go-translate)
;;   :config
;;   (setq gts-translate-list '(("de" "en")
;;                              ("en" "de")))
;;   (setq gts-default-translator
;;         (gts-translator
;;          :picker (gts-prompt-picker)
;;          :engines (list (gts-bing-engine) (gts-google-engine))
;;          :render (gts-buffer-render)))
;; )


;;; binky
;; https://github.com/liuyinz/binky.el/

;; (use-package binky
;;   :init
;;   (setq binky-overwrite t)
;;   (binky-mode)
;;   (binky-margin-mode)
;;   )

;;; string-edit-at-point
;; https://github.com/magnars/string-edit.el

(use-package string-edit-at-point
  :defer t)

;;; substitute
;; https://protesilaos.com/emacs/substitute

(use-package substitute

  :config
  (setq substitute-fixed-letter-case t)

  :bind
  (:map witek-context-key-map
        ("s b" . 'substitute-target-in-buffer)
        ("s d" . 'substitute-target-in-defun))
  )

;; server

(use-package server
  :ensure nil
  :defer 1
  :config (unless (server-running-p)
            (server-start)))

;;; edit-server
;; https://github.com/stsquad/emacs_chrome

(use-package edit-server
  :defer t
  :commands edit-server-start
  :init
  (add-hook 'after-init-hook
            #'(lambda () (edit-server-start)))
  :config
  (setq edit-server-new-frame-alist
        '((name . "Edit with Emacs FRAME")
          (top . 200)
          (left . 200)
          (width . 80)
          (height . 25)
          (minibuffer . t)
          (menu-bar-lines .t)
          (window-system . x))))

;;; gmail-message-mode
;; https://github.com/Malabarba/gmail-mode

(use-package gmail-message-mode
  :defer t)

;;; chatgpt-shell
;; https://github.com/xenodium/chatgpt-shell

(use-package chatgpt-shell
  :defer t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-frankenburg")))))

;; (use-package dall-e-shell
;;   :custom
;;   ((dall-e-shell-openai-key
;;     (lambda ()
;;       (auth-source-pass-get 'secret "openai-frankenburg")))))

(use-package ob-chatgpt-shell
  :defer t
  ;; :init
  ;; (setq org-babel-load-languages '((chatgpt-shell . t)))
  :config
  (ob-chatgpt-shell-setup)
  )

;;;

;; (use-package app-launcher
;; )


;;; multifiles
;; https://github.com/magnars/multifiles.el
;; !!! BUGGY !!!
;; (use-package multifiles
;;   :bind ("C-m" . mf/mirror-region-in-multifile)
;;   )

;;; provide
(provide 'my-extras)
