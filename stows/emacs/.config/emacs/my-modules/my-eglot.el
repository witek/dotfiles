;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(defun my/eglot-rename ()
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
        ("r" . 'my/eglot-rename))

  :config
  (add-hook 'clojure-mode-hook #'eglot-ensure)

  )

;;; provide

(provide 'my-eglot)
