;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(defun my/hg-text-wrap (text-key)
  (interactive "sEnter key for text:")
  (save-excursion
   (sp-beginning-of-sexp)
   (backward-char)
   ;; (insert "#")
   (sp-wrap-round)
   (insert "u/text :")
   (insert text-key)
   (insert " "))
  (save-buffer)
  )
(define-key witek-context-key-map (kbd "t") 'my/hg-text-wrap)

(defun my/wrap-round ()
  (interactive)
  (sp-wrap-round)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "(") 'my/wrap-round)

(defun my/wrap-square ()
  (interactive)
  (sp-wrap-square)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "[") 'my/wrap-square)

(defun my/wrap-curly ()
  (interactive)
  (sp-wrap-curly)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "{") 'my/wrap-curly)

(defun witek-meow-mark-symbol ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'meow-next-symbol)
    (call-interactively 'meow-mark-symbol)))

(defun witek-meow-mark-word ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'meow-next-word)
    (call-interactively 'meow-mark-word)))

(defun my/append-after-end-of-sexp ()
  (interactive)
  (sp-end-of-sexp)
  (insert " ")
  (call-interactively 'meow-insert))

(defun my/before-beginning-of-sexp ()
  (interactive)
  (sp-beginning-of-sexp)
  (backward-char))

(defun my/after-end-of-sexp ()
  (interactive)
  (sp-end-of-sexp)
  (forward-char))

(defun my/indent-region-or-defun ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'meow-indent)
    (call-interactively 'sp-indent-defun)))

(defun my/matching-paren ()
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ))

;;; provide
(provide 'witek-commands)
