

;; hack for doom-modeline internal var access
;; (defun eglot--major-mode (server) (car (eglot--major-modes server)))




(defun witek-eglot-rename ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'eglot-rename)
  (save-some-buffers t)
  )


(use-package eglot
  :demand t

  :init
  (customize-set-variable `eglot-confirm-server-initiated-edits nil)
  (setq eglot-connect-timeout 120)
  
  :bind
  (:map witek-context-key-map
        ("r" . 'witek-eglot-rename))

  :config
  (add-hook 'clojure-mode-hook #'eglot-ensure)

  )


;; (setq project-find-functions '(witek-project-find-for-clj-project))

;; * Export witek-eglot
(provide 'my-eglot)
