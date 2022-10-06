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


;; ** lsp-mode - IDE

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix ", r")
  (setq lsp-auto-guess-root t)

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

  :hook ((clojure-mode . lsp))
  :commands lsp)

;; * Export witek-lsp
(provide 'witek-lsp)
