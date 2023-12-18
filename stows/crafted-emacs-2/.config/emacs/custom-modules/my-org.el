;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(use-package org
  :config
  (setq org-directory "~/org/")
  (setq org-archive-location "~/org/archive.org")
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-return-follows-link t)
  (setq org-blank-before-new-entry t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-reverse-note-order t)

  ;; TODO more TODO keywords
  
   )

(use-package org-modern
  :config
  (global-org-modern-mode 1))

;;; provide
(provide 'my-org)
