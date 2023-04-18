;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>


;;; which-key

(crafted-package-install-package 'which-key)
(use-package which-key
  :config
  (setq which-key-show-transient-maps t)
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  )

;;; hi-lock

(use-package hi-lock
  :bind (:map witek-context-key-map
              ("h s"        . 'highlight-symbol-at-point)
              ("h <escape>" . 'witekunhighlight-all-in-buffer))
  :config

  (defun witek-unhighlight-all-in-buffer ()
    "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
    (interactive)
    (unhighlight-regexp t))
  )

;;; corfu

(use-package corfu
  :bind (:map corfu-map
              ("C-ä"     . 'corfu-quit)
              ("<ret>"   . nil)
              ("<right>" . 'corfu-insert)
              ))

;;; consult

(use-package consult
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; embark-consult

(use-package embark-consult
  :bind (:map vertico-map
              ("C-, e" . embark-export)
              ("C-, a" . embark-act)))

;;; magit

(crafted-package-install-package 'magit)
(use-package magit
  :bind (("C-c g s" . 'magit-status)
         ("C-c G s" . 'magit-status)

         :map witek-context-key-map
         ("M"       . 'magit-file-dispatch)

         :map magit-status-mode-map
         ("x"       . 'magit-discard)))

;;; clean-kill-ring

;; (crafted-package-install-package 'clean-kill-ring)
;; (use-package clean-kill-ring
;;   ;; :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
;;   :config
;;   (clean-kill-ring-mode 1))

;;; treemacs

(crafted-package-install-package 'treemacs)
(use-package treemacs
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

;;; smartparens

(crafted-package-install-package 'smartparens)
(use-package smartparens
  :demand t
  :bind (("<backspace>" . 'sp-backward-delete-char)

         ("M-l" . 'sp-forward-slurp-sexp) ; owerride: downcase-word
         ("M-h" . 'sp-forward-barf-sexp)  ; override: mark-paragraph

         ("M-w" . 'sp-clone-sexp)         ; override: kill-ring-save
         ("H-w" . 'kill-ring-save)

         :map witek-context-key-map
         ("=" . 'sp-indent-defun)
         )

  :config

  ;; fix, so that \n is not added to the kill-ring
  (defun sp-kill-whole-line ()
    (interactive)
    (if mark-active
        (sp-kill-region (region-beginning) (region-end))
      (beginning-of-line)
      (sp-kill-hybrid-sexp nil)
      (when (= (char-after) ?\n)
        (append-next-kill)
        (kill-whole-line))))

  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  )

;;; popper

;; https://github.com/karthink/popper

(crafted-package-install-package 'popper)
(use-package popper
  :init
  (setq popper-reference-buffers
        '(help-mode))
  (popper-mode 1)
  (popper-echo-mode 1))

;;; restart-emacs

(crafted-package-install-package 'restart-emacs)
(use-package restart-emacs
  :bind (("C-c e r" . 'restart-emacs))
  )


;;; wgrep

;; https://github.com/mhayashi1120/Emacs-wgrep

(crafted-package-install-package 'wgrep)
(use-package wgrep
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


;;; avy

;; https://github.com/abo-abo/avy

(use-package avy
  :ensure t
  :bind ("C-j" . avy-goto-char-timer))

;;; adoc-mode

(crafted-package-install-package 'adoc-mode)
(use-package adoc-mode
  )


;;; clojure-mode

(use-package clojure-mode
  :ensure t
  :config
  (setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically 't
   )

  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  )

;;; cider

(use-package cider
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp)
  (setq cider-font-lock-reader-conditionals nil)
  (setq cider-auto-inspect-after-eval t)
  (setq cider-save-file-on-load t)

  (define-key witek-context-key-map (kbd "e b") 'cider-eval-buffer)
  (define-key witek-context-key-map (kbd "e l") 'cider-eval-last-sexp)
  (define-key witek-context-key-map (kbd "e s") 'cider-eval-region)

  )

;;; clj-refactor

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))

;;; figlet

(crafted-package-install-package 'figlet)
(use-package figlet
  )

;;; gptel

;; https://github.com/karthink/gptel

(crafted-package-install-package 'gptel)
(use-package gptel
  :config
  (setq gptel-api-key
        (funcall
         (plist-get
          (car
           (auth-source-search :host "platform.openai.com"))
          :secret))))

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
(provide 'witek-extras)
