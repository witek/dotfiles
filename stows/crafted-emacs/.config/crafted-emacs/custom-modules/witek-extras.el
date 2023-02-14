;;; witek-extras.el --- Witek's Extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>




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

;; * Export witek-extras
(provide 'witek-extras)
