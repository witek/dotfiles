;;; witek-extras.el --- Witek's Extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>


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
  (global-set-key (kbd "C-c e r") 'restart-emacs))


;; * wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep

(use-package wgrep
  :straight t
  :after embark-consult
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :config
  (my-local-leader-def
    :keymaps '(grep-mode-map wgrep-mode-map)
    "e" 'wgrep-change-to-wgrep-mode
    "," 'wgrep-finish-edit
    "q" 'wgrep-exit))

;; * Avy
;; https://github.com/abo-abo/avy

(use-package avy
  :straight t
  :ensure t
  :bind ("C-j" . avy-goto-char-timer))

;; * AsciiDoc

(use-package adoc-mode
  :straight t)

;; * Clojure, CIDER clj-refactor

(use-package cider
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp))

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))





;; * Export witek-extras
(provide 'witek-extras)
