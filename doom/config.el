;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;           DO NOT MODIFY!
;;       tangeled from config.org

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Witoslaw Koczewski"
      user-mail-address "wi@koczewski.de")

(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))




;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq display-line-numbers-type nil)

(setq scroll-conservatively 101
      scroll-margin 12
      scroll-preserve-screen-position 't)

(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      ivy-posframe-font (font-spec :family "Fira Code" :size 17))

(setq confirm-kill-emacs nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(map! :leader
      :desc "List commands"
      "SPC"  #'execute-extended-command)

(map! :map global-map
      :n "C-h" #'evil-prev-buffer
      :n "C-l" #'evil-next-buffer
      :n "C-j" #'evil-jump-forward
      :n "C-k" #'evil-jump-backward
      )

(map! :map global-map
      :nv ";" #'evilnc-comment-or-uncomment-lines)

(map! :map global-map
      :n "M-h"  #'paredit-forward-barf-sexp)
(map! :map global-map
      :n "M-l"  #'paredit-forward-slurp-sexp)

;; (map! :localleader
      ;; ",a"  #'evil-cp-insert-at-end-of-form
      ;; ",i" 'evil-cp-insert-at-beginning-of-form
      ;; "(" #'sp-wrap-round)

(print "[config.org] Org")

(setq org-directory "~/org/")

(print "[config.org] LSP")

(with-eval-after-load 'lsp-mode
  (print "[config.org] with-eval-after-load lsp-mode")
  (setq lsp-ui-imenu-enable t
        lsp-ui-doc-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-lens-enable t
        lsp-enable-symbol-highlighting nil
        ;; lsp-enable-file-watchers nil
        ;; +lsp-prompt-to-install-server 'quiet
        )
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\firebase\\'")
  )

(print "[config.org] Clojure")

(map! :localleader
      :mode clojure-mode
      ;; "==" 'lsp-format-buffer
      ;; "(" 'sp-wrap-round
      ;; "#" 'cider-toggle-ignore-next-form
      "ev" #'cider-eval-sexp-at-point)

(map! :localleader
      :mode clojurescript-mode
      ",a" 'evil-cp-insert-at-end-of-form
      ",i" 'evil-cp-insert-at-beginning-of-form
      "ev" #'cider-eval-sexp-at-point)

(map! :localleader
      :mode clojurec-mode
      "ev" #'cider-eval-sexp-at-point)
