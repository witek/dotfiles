;;; -*- lexical-binding: t; -*-


(require 'crafted-org)


(use-package org
  :init
  (setq org-directory "~/org/")

  :config

  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))


    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  )



;; * outshine - Org features in code files
;; ** dummy
;; - a
;; - b
;; [[https://github.com/alphapapa/outshine]]
;; [[https://orgmode.org/guide/Hyperlinks.html]]

(crafted-package-install-package 'outshine)
;; (use-package outshine
;;   :init

;;   (defun my/activate-outshine ()
;;     (outshine-mode 1))

;;   (add-hook 'emacs-lisp-mode-hook 'my/activate-outshine)
;;   (add-hook 'clojure-mode-hook 'my/activate-outshine))

;; * origami

;; (use-package origami
;;   )
;; (global-origami-mode 1)


(provide 'witek-org)
