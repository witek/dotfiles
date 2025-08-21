;; -*- lexical-binding: t; -*-

(defun witek-lsp-rename ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-rename)
  (save-some-buffers t)
  )

(defun witek-lsp-organize-imports ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-organize-imports)
  (save-some-buffers t)
  (call-interactively 'lsp-organize-imports)
  (save-some-buffers t)
  )

(defun witek-lsp-clojure-clean-ns ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-clojure-clean-ns)
  (save-some-buffers t)
  )

;;; lsp-mode

(use-package lsp-mode
  :ensure t
  :demand t
  :hook ((clojure-mode . lsp))
  :bind
  (:map witek-context-key-map
        ("r" . 'witek-lsp-rename))

  :config

  (setq lsp-file-watch-threshold 5000)
  ;; (setq lsp-keymap-prefix ", r")

  ;; We don't want to get asked about project root
  (setq lsp-auto-guess-root t)

  ;; Because we don't use company
  (setq lsp-completion-provider :none)

  (setq lsp-enable-indentation t)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-log-io nil)

  ;; Don't underline on errors/warnings
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)

  (setq lsp-lens-enable t)
  ;; (setq lsp-lens-place-position 'above-line)

  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold :height 0.7)))
  ;;  '(lsp-lens-face ((t (:inherit lsp-details-face :height 0.7)))))

  ;; (setq lsp-headerline-breadcrumb-path-face '((t :inherit font-lock-string-face :height 0.5)))

  (lsp-semantic-tokens--warn-about-deprecated-setting)

  )

;;; lsp-ui

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  ;; :demand t
  ;; :commands lsp-ui-mode


  :config

  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-use-webkit t)
  
  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold :height 0.7)))
  ;;  '(lsp-lens-face ((t (:inherit lsp-details-face :height 0.7))))
  ;;  )

  )

;;; lsp-treemacs

(use-package lsp-treemacs
  :ensure t
  :defer t
  :commands lsp-treemacs-errors-list)

;;; consult-lsp

(use-package consult-lsp
  :ensure t
  :defer t
  :after (consul lsp-mode)
  )

;;; provide
(provide 'my-lsp)
