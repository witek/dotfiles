;; Based on Crafted Emacs and davivil's config.

;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

(require 'witek-defaults)
;; (require 'witek-eglot)
(require 'witek-lsp)

;; ** Clojure

;; *** smartparens

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'evil-smartparens-mode)

;; *** defaults

(setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically 't
   )

(setq cider-font-lock-reader-conditionals nil)
(setq cider-auto-inspect-after-eval t)
(setq cider-save-file-on-load t)

;; ** Org

(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.

(setq org-directory "~/org/")

;; *** outshine - Org features in code files
;; [[https://github.com/alphapapa/outshine]]
;; [[https://orgmode.org/guide/Hyperlinks.html]]
;; (use-package outshine
;;   :straight t
;;   :init

;;   (defun witek-activate-outshine ()
;;     (outshine-mode 1)
;;     (general-define-key
;;      :keymaps 'outline-mode-map
;;      :states 'normal
;;      "M-l" 'nil
;;      "M-h" 'nil))

;;   (add-hook 'emacs-lisp-mode-hook 'witek-activate-outshine)
;;   (add-hook 'clojure-mode-hook 'witek-activate-outshine))

;; ** origami

;; (use-package origami
;;   :straight t)
;; (global-origami-mode 1)

;; ** Witek's extensions

;; *** witek-make-frame-with-messages

(defun witek-make-frame-with-messages ()
  "Make a new frame with *Messages* buffer."
  (interactive)
  ;; (view-buffer-other-frame "*Messages*")
  (make-frame)
  (view-buffer "*Messages*")
  ;; (let ((current-frame (selected-frame)))
  ;;   (select-frame current-frame))
  )

(my-leader-def
  "e m" 'witek-make-frame-with-messages)

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

(my-leader-def
  "f D" 'witek-delete-current-file)

;; *** witek-save-all-buffers

(defun witek-save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers t))

(my-leader-def
  "f S" 'witek-save-all-buffers)

(defun witek-rename-file (new-name)

  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")

  (let ((name (buffer-name))

        (filename (buffer-file-name)))

    (if (not filename)

        (message "Buffer '%s' is not visiting a file!" name)

      (if (get-buffer new-name)

          (message "A buffer named '%s' already exists!" new-name)

        (progn   (rename-file filename new-name 1)   (rename-buffer new-name)   (set-visited-file-name new-name)   (set-buffer-modified-p nil)))))) ;;

(my-leader-def
  "f R" 'witek-rename-file)

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

(my-leader-def
  "f M" 'witek-move-file)
