;;; elpaca

(load (concat user-emacs-directory "init-elpaca.el"))

;;; custom-file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;;; ef-themes

(use-package ef-themes
  :config
  (ef-themes-select 'ef-maris-dark)
  )

;;; meow

(load (concat user-emacs-directory "init-meow.el"))


