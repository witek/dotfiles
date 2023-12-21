;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; eldoc

(require 'eldoc)

;;; aggressive-indent-mode

;; (when (locate-library "aggressive-indent")
  ;; (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'scheme-mode-hook #'aggressive-indent-mode))

;;; linum-relative
;; https://github.com/coldnew/linum-relative

(require 'linum-relative)
(setq linum-relative-backend 'display-line-numbers-mode)
(linum-relative-global-mode 1)

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

  :bind (:map witek-context-key-map
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

;;; dirvish

(use-package dirvish
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

(use-package phi-search
  :config
  (global-set-key (kbd "C-s") 'phi-search)
  (global-set-key (kbd "C-r") 'phi-search-backward)
  )

;;; paren-face
;; https://github.com/tarsius/paren-face

(use-package paren-face
  :init
  (global-paren-face-mode 1)
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

;; (use-package wgrep
  ;; :after embark-consult
  ;; :init
  ;; (setq wgrep-auto-save-buffer t)
  ;; (setq wgrep-change-readonly-file t)
  ;; :config
  ;; (my-local-leader-def
  ;;   :keymaps '(grep-mode-map wgrep-mode-map)
  ;;   "e" 'wgrep-change-to-wgrep-mode
  ;;   "," 'wgrep-finish-edit
  ;;   "q" 'wgrep-exit)
  ;; )

;;; avy
;; https://github.com/abo-abo/avy

(use-package avy
  :bind ("C-j" . avy-goto-char-timer))

;;; adoc-mode

(use-package adoc-mode
  )


;;; clojure-mode

(use-package clojure-mode
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
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp)
  (setq cider-font-lock-reader-conditionals nil)
  (setq cider-auto-inspect-after-eval t)
  (setq cider-save-file-on-load t)

  (define-key witek-context-key-map (kbd "e b") 'cider-eval-buffer)
  (define-key witek-context-key-map (kbd "e l") 'cider-eval-last-sexp)
  (define-key witek-context-key-map (kbd "e s") 'cider-eval-region)

  )

;;; clj-refactor

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))

;;; figlet
;; https://www.emacswiki.org/emacs/Figlet

(use-package figlet
  )

;;; gptel
;; https://github.com/karthink/gptel

;; (use-package gptel
;;   :config
;;   (setq gptel-api-key
;;         (funcall
;;          (plist-get
;;           (car
;;            (auth-source-search :host "platform.openai.com"))
;;           :secret))))

;;; lorem-ipsum

(use-package lorem-ipsum)

;;; go-translate

(use-package go-translate
  :init
  (require 'go-translate)
  :config
  (setq gts-translate-list '(("de" "en")
                             ("en" "de")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine) (gts-google-engine))
         :render (gts-buffer-render)))
  )

;;; org

;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Visually indent org-mode files to a given header level
(add-hook 'org-mode-hook #'org-indent-mode)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(when (locate-library "org-appear")
  (add-hook 'org-mode-hook 'org-appear-mode))

;;; binky
;; https://github.com/liuyinz/binky.el/

(use-package binky
  :init
  (setq binky-overwrite t)
  (binky-mode)
  (binky-margin-mode)
  )

;;; string-edit-at-point
;; https://github.com/magnars/string-edit.el

(use-package string-edit-at-point)

;;; substitute
;; https://protesilaos.com/emacs/substitute

(use-package substitute
  :config
  (setq substitute-fixed-letter-case t))

;;; edit-server
;; https://github.com/stsquad/emacs_chrome

(use-package edit-server
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

(use-package gmail-message-mode)

;;; chatgpt-shell
;; https://github.com/xenodium/chatgpt-shell

(use-package chatgpt-shell
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-frankenburg")))))

;;; provide
(provide 'my-extras)









