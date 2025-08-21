;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; Emacs

(use-package emacs
  :bind ("C-j" . join-line))

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


;;; vertico
;; https://github.com/minad/vertico

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)

  :config
  (setq vertico-cycle t)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  
  )


;;; marginalia
;; https://github.com/minad/marginalia

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))


;;; consult
;; https://github.com/minad/consult

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :bind (
         :map vertico-map
         ("C-, p" . consult-preview-at-point-mode)

         :map minibuffer-local-map
         ("C-r" . 'consult-history)     
         ))


;;; orderless
;; https://github.com/oantolin/orderless

(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles . (partial-completion)))))
  ;; (setq completion-category-defaults nil)
  )


;;; embark
;; https://github.com/oantolin/embark

(use-package embark
  :ensure t

  :config

  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command)

  
  :bind (
         :map global-map
         ("C-." . 'embark-act)

         :map witek-context-key-map
         ("<RET>" . embark-dwim)
         ("e a" . 'embark-act)

         :map vertico-map
         ("C-e" . embark-export)
         ("C-, e" . embark-export)
         ("C-, a" . embark-act)

         )
  )

;;; embark-consult

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
  :after (embark consult)
  )


;;; corfu
;; https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 4)
  (setq corfu-auto-delay 0.8)
  (setq corfu-quit-no-match 'separator)

  (corfu-popupinfo-mode 1)
  
  :bind (:map corfu-map
              ("RET" . nil)
              ("<tab>"  . 'corfu-insert)
              ("<right>"  . 'corfu-insert)))


;;; nerd-icons-corfu
;; https://github.com/LuigiPiucco/nerd-icons-corfu

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)))
  )


;;; cape
;; https://github.com/minad/cape

(use-package cape
  :ensure t
  :after corfu
  
  :config
  
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

   ;; No auto-completion or completion-on-quit in eshell
  (defun my/completion-corfu-eshell ()
    "Special settings for when using corfu with eshell."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook #'my/completion-corfu-eshell)
  
  )


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
