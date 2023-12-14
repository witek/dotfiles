;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

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
        '(help-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

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
   clojure-align-forms-automatically 't
   )

  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
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


;;; provide
(provide 'my-extras)