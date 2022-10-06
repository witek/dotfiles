;; * lsp-mode

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix ", r")

  ;; We don't want to get asked about project root
  (setq lsp-auto-guess-root t)

  ;; Don't underline on errors/warnings
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)

  (setq lsp-lens-enable t)
  (setq lsp-lens-place-position 'above-line)

  (custom-set-faces
   '(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.7)))
   '(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face :height 0.7)))
   '(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold :height 0.7)))
   '(lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold :height 0.7)))
   '(lsp-lens-face ((t (:inherit lsp-details-face :height 0.7)))))

  ;; (setq lsp-headerline-breadcrumb-path-face '((t :inherit font-lock-string-face :height 0.5)))

  :config
  (defun lsp--suggest-project-root ()
    "Get project root."
    (let ((dir (project-root (project-current))))
      (message "[witek:lsp--suggest-project-root] project=%s dir=%s" (project-current) dir)
      (if (or (string-prefix-p "/p/happygast/" dir)
              (string-prefix-p "/p/incubator/" dir)
              (string-prefix-p "/p/spark/" dir)
              (string-prefix-p "/p/kunagi-mui/" dir)
              (string-prefix-p "/p/kunagi-utils/" dir))
          (project-root (project-try-vc "/p/clj/"))
        (project-root (project-try-vc dir)))
      ))

  ;; (add-hook 'lsp-after-apply-edits-hook (lambda (arg)
  ;;                                         (message "[witek-hook] save-all-buffers %s" arg)
  ;;                                         (witek-save-all-buffers)
  ;;                                         (message "[witek-hook] saved")
  ;;                                         ))

  :hook ((clojure-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :init
  ;; (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-sideline-show-code-actions t)

  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'top)

  )

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)


;; * Clojure

(my-local-leader-def
  :keymaps 'clojure-mode-map
  "e v" 'cider-eval-sexp-at-point
  "e b" 'cider-eval-buffer
  )

(general-define-key
 :keymaps 'clojure-mode-map
 :states 'normal
 "g r" 'lsp-find-references
 "g d" 'lsp-find-definition)


;; * Export witek-lsp
(provide 'witek-lsp)
