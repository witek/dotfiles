;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(defun my/hg-text-wrap (text-key)
  (interactive "sEnter key for text:")
  (save-excursion
   (sp-beginning-of-sexp)
   (backward-char)
   (paredit-wrap-round)
   (insert "u/text :")
   (insert text-key)
   (insert " ")

   ;; (sp-beginning-of-sexp)
   ;; (backward-char)
   ;; (insert "#")
   )
  (save-buffer)
  )
(define-key witek-context-key-map (kbd "t") 'my/hg-text-wrap)

(defun my/wrap-round ()
  (interactive)
  (paredit-wrap-round)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "(") 'my/wrap-round)

(defun my/wrap-square ()
  (interactive)
  (paredit-wrap-square)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "[") 'my/wrap-square)

(defun my/wrap-curly ()
  (interactive)
  (paredit-wrap-curly)
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

;;; witek

(defun witek-make-frame-with-messages ()
  "Make a new frame with *Messages* buffer."
  (interactive)
  ;; (view-buffer-other-frame "*Messages*")
  (make-frame)
  (view-buffer "*Messages*")
  ;; (let ((current-frame (selected-frame)))
  ;;   (select-frame current-frame))
  )

;;; witek-kill-other-buffers

(defun witek-project-kill-other-buffers ()
  "kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (get-buffer "*lsp-log*")
              (delq (current-buffer)
                    (project-buffers (project-current))))))

;; *** witek-delete-file

(defun witek-delete-current-file ()
  "Delete the current buffer and file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; *** witek-save-all-buffers

(defun witek-save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))

(defun witek-rename-file (new-name)

  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")

  (let ((name (buffer-name))

        (filename (buffer-file-name)))

    (if (not filename)

        (message "Buffer '%s' is not visiting a file!" name)

      (if (get-buffer new-name)

          (message "A buffer named '%s' already exists!" new-name)

        (progn   (rename-file filename new-name 1)   (rename-buffer new-name)   (set-visited-file-name new-name)   (set-buffer-modified-p nil)))))) ;;

(defun witek-move-file (dir)

  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")

  (let* ((name (buffer-name))

         (filename (buffer-file-name))

         (dir

          (if (string-match dir "\\(?:/\\|\\\\)$")

              (substring dir 0 -1) dir))

         (newname (concat dir "/" name)))

    (if (not filename)

        (message "Buffer '%s' is not visiting a file!" name)

      (progn  (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)  (set-buffer-modified-p nil)  t))))


;;; provide
(provide 'my-commands)






