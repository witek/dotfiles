;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; corfu

(use-package corfu
  :config
  (setq corfu-auto nil
        corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("RET" . nil)
              ("<right>"  . 'corfu-insert)))

;;; consult

(use-package consult
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; embark

(use-package embark
  :bind (:map witek-context-key-map
              ("e a" . 'embark-act)))

;;; embark-consult

(use-package embark-consult
  :bind (:map
         vertico-map
         ("C-, e" . embark-export)
         ("C-, a" . embark-act)

         :map
         witek-context-key-map
         ("<RET>" . embark-dwim)
         ))


;;; smartparens

(use-package smartparens
  :bind (("<backspace>" . 'sp-backward-delete-char)

         ("M-l" . 'sp-forward-slurp-sexp) ; owerride: downcase-word
         ("M-h" . 'sp-forward-barf-sexp)  ; override: mark-paragraph

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
  :defer
  :bind (("C-c g s" . 'magit-status)
         ("C-c G s" . 'magit-status)

         :map witek-context-key-map
         ("M"       . 'magit-file-dispatch)

         :map magit-status-mode-map
         ("x"       . 'magit-discard))
  )

;;; provide
(provide 'my-basics)




















