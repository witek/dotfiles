;;; witek-extras.el --- Witek's Extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>


(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package magit
  :straight t
  :bind (("C-c g s" . 'magit-status)))

(use-package clean-kill-ring
  :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
  :config
  (clean-kill-ring-mode 1))

;; * treemacs

(use-package treemacs
  :straight t

  :init
  (setq treemacs-space-between-root-nodes nil)

  :config
  ;; (my-local-leader-def
  ;;   :keymaps (list 'treemacs-mode-map)
  ;;   "," 'treemacs-mark-or-unmark-path-at-point
  ;;   "x" 'treemacs-delete-file
  ;;   "m" 'treemacs-move-file
  ;;   "c" 'treemacs-copy-file
  ;;   "r" 'treemacs-rename-file
  ;;   )
  )

;; * Restart Emacs

(use-package restart-emacs
  :straight t
  :config
  (global-set-key (kbd "C-c e r") 'restart-emacs))


;; * wgrep
;; https://github.com/mhayashi1120/Emacs-wgrep

(use-package wgrep
  :straight t
  :after embark-consult
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :config
  ;; (my-local-leader-def
  ;;   :keymaps '(grep-mode-map wgrep-mode-map)
  ;;   "e" 'wgrep-change-to-wgrep-mode
  ;;   "," 'wgrep-finish-edit
  ;;   "q" 'wgrep-exit)
  )

;; * Avy
;; https://github.com/abo-abo/avy

(use-package avy
  :straight t
  :ensure t
  :bind ("C-j" . avy-goto-char-timer))

;; * AsciiDoc

(use-package adoc-mode
  :straight t)

;; * Clojure, CIDER clj-refactor

(use-package cider
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp))

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))



;; ** Clojure

;; *** smartparens

(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;; *** defaults

(setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically 't
   )

(setq cider-font-lock-reader-conditionals nil)
(setq cider-auto-inspect-after-eval t)
(setq cider-save-file-on-load t)

;; ** Org


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



;; * Export witek-extras
(provide 'witek-extras)
