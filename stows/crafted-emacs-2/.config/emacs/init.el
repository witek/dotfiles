;; https://github.com/SystemCrafters/crafted-emacs

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(customize-set-variable 'crafted-init-auto-save-customized nil)
(customize-set-variable 'crafted-init-auto-save-selected-packages nil)

(load "~/crafted-emacs/modules/crafted-init-config")

;;; Install packages

(require 'crafted-completion-packages)
(require 'crafted-ui-packages)
(require 'crafted-lisp-packages)
(require 'crafted-org-packages)
(require 'crafted-writing-packages)

(add-to-list 'package-selected-packages 'vertico)
(add-to-list 'package-selected-packages 'cape)
(add-to-list 'package-selected-packages 'consult)
(add-to-list 'package-selected-packages 'corfu)
(add-to-list 'package-selected-packages 'embark)
(add-to-list 'package-selected-packages 'ef-themes)

(package-install-selected-packages :noconfirm)

;;; Configuration

(require 'crafted-updates-config)
(require 'crafted-defaults-config)
(require 'crafted-startup-config)
(require 'crafted-ui-config)
(require 'crafted-completion-config)
(require 'crafted-lisp-config)
(require 'crafted-org-config)
(require 'crafted-writing-config)
