;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; org

;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Visually indent org-mode files to a given header level
(add-hook 'org-mode-hook #'org-indent-mode)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(when (locate-library "org-appear")
  (add-hook 'org-mode-hook 'org-appear-mode))

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
  :after '(org)
  :config
  (global-org-modern-mode))

(use-package outshine
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-mode))

;;; denote
;; https://protesilaos.com/emacs/denote#h:5d16932d-4f7b-493d-8e6a-e5c396b15fd6

(use-package denote
  :defer t
  :config
  (setq denote-directory (expand-file-name "~/myfiles/denote/"))
  (setq denote-save-buffer-after-creation nil)
  (setq denote-known-keywords '("emacs" "dev" "home" "happygast" "frankenburg"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-no-confirm t)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (denote-rename-buffer-mode 1)
  (add-hook 'context-menu-functions #'denote-context-menu)
  )

;;; provide
(provide 'my-org)
