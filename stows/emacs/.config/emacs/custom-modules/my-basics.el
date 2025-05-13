;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; isearch
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html
(use-package isearch
  :config
  (setq isearch-repeat-on-direction-change t)
  (setq isearch-wrap-pause 'no)
  :bind
  (:map global-map
        ("C-s" . 'isearch-forward)
        ("C-r" . 'isearch-backward))
  )

;;; orderless

(use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion)))))
    )

;;; corfu
;; https://github.com/minad/corfu

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.8)
  (corfu-auto-prefix 4)
  (corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("RET" . nil)
              ("<tab>"  . 'corfu-insert)
              ("<right>"  . 'corfu-insert)))

;;; nerd-icons-corfu
;; https://github.com/LuigiPiucco/nerd-icons-corfu

(use-package nerd-icons-corfu
  :defer t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)))
  )

;;; consult

(use-package consult
  :defer t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind (:map
         vertico-map
         ("C-, p" . consult-preview-at-point-mode)
         ))

;;; embark

(use-package embark
  :defer t
  :bind (:map witek-context-key-map
              ("e a" . 'embark-act)))

;;; embark-consult

(use-package embark-consult
  :defer t
  :bind (:map
         vertico-map
         ("C-e" . embark-export)
         ("C-, e" . embark-export)
         ("C-, a" . embark-act)

         :map
         witek-context-key-map
         ("<RET>" . embark-dwim)
         ))


;;; smartparens

(use-package smartparens
  :bind (("<backspace>" . 'sp-backward-delete-char)

         ("C-l" . 'sp-forward-slurp-sexp)
         ("C-h" . 'sp-forward-barf-sexp)

         ;; ("M-w" . 'sp-clone-sexp)         ; override: kill-ring-save
         ;; ("H-w" . 'kill-ring-save)

         :map witek-context-key-map
         ("s c" . 'sp-clone-sexp)
         ("s s" . 'sp-split-sexp)
         ("=" . 'sp-indent-defun)
         )

  :config

  (require 'smartparens-config)
  (show-smartparens-global-mode 1)

  ;; fix, so that \n is not added to the kill-ring
  (defun sp-kill-whole-line ()
    (interactive)
    (if mark-active
        (sp-kill-region (region-beginning) (region-end))
      (beginning-of-line)
      (sp-kill-hybrid-sexp nil)
      (when (= (char-after) ?\n)
        (append-next-kill)
        (kill-whole-line))))

  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  )

;;; magit

;; (use-package compat)
;; (use-package dash)
;; (use-package transient)
;; (use-package with-editor)

(use-package magit
  :defer t
  :bind (("C-c g s" . 'magit-status)
         ("C-c G s" . 'magit-status)

         :map witek-context-key-map
         ("M"       . 'magit-file-dispatch)

         :map magit-status-mode-map
         ("x"       . 'magit-discard))
  )

;;; provide
(provide 'my-basics)
