
(customize-set-variable `eglot-confirm-server-initiated-edits nil)

;; hack for doom-modeline internal var access
;; (defun eglot--major-mode (server) (car (eglot--major-modes server)))

(add-hook 'clojure-mode-hook #'eglot-ensure)

;; *** /p/clj/ as eglot root

(defun witek-project-find-for-clj-project (dir)
  (if (and (boundp 'eglot-lsp-context)
           eglot-lsp-context
           (or (string-prefix-p "/p/happygast/" dir)
               (string-prefix-p "/p/incubator/" dir)
               (string-prefix-p "/p/spark/" dir)
               (string-prefix-p "/p/kunagi-mui/" dir)
               (string-prefix-p "/p/kunagi-utils/" dir)))
      (project-try-vc "/p/clj/")
    (project-try-vc dir)))

;; (setq project-find-functions '(witek-project-find-for-clj-project))

;; * Export witek-eglot
(provide 'my-eglot)
