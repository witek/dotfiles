;;; witek-extras.el --- Witek's Extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>



;;           _     _      _           _
;; __      _| |__ (_) ___| |__       | | _____ _   _
;; \ \ /\ / / '_ \| |/ __| '_ \ _____| |/ / _ \ | | |
;;  \ V  V /| | | | | (__| | | |_____|   <  __/ |_| |
;;   \_/\_/ |_| |_|_|\___|_| |_|     |_|\_\___|\__, |

(crafted-package-install-package 'which-key)
(use-package which-key
  :config
  (setq which-key-show-transient-maps t)
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  )

;;  _     _       _            _
;; | |__ (_)     | | ___   ___| | __
;; | '_ \| |_____| |/ _ \ / __| |/ /
;; | | | | |_____| | (_) | (__|   <
;; |_| |_|_|     |_|\___/ \___|_|\_\

(use-package hi-lock
  :bind (:map witek-context-key-map
              ("h s"        . 'highlight-symbol-at-point)
              ("h <escape>" . 'my/unhighlight-all-in-buffer))
  :config

  (defun my/unhighlight-all-in-buffer ()
    "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
    (interactive)
    (unhighlight-regexp t))
  )

;;                  __
;;   ___ ___  _ __ / _|_   _
;;  / __/ _ \| '__| |_| | | |
;; | (_| (_) | |  |  _| |_| |
;;  \___\___/|_|  |_|  \__,_|

(use-package corfu
  :bind (:map corfu-map
              ("C-ä"     . 'corfu-quit)
              ("<ret>"   . nil)
              ("<right>" . 'corfu-insert)
              ))

;;                 _                _                                   _ _
;;   ___ _ __ ___ | |__   __ _ _ __| | __      ___ ___  _ __  ___ _   _| | |_
;;  / _ \ '_ ` _ \| '_ \ / _` | '__| |/ /____ / __/ _ \| '_ \/ __| | | | | __|
;; |  __/ | | | | | |_) | (_| | |  |   <_____| (_| (_) | | | \__ \ |_| | | |_
;;  \___|_| |_| |_|_.__/ \__,_|_|  |_|\_\     \___\___/|_| |_|___/\__,_|_|\__|

(use-package embark-consult
  :bind (:map vertico-map
              ("C-, e" . embark-export)
              ("C-, a" . embark-act)))

;;                        _ _
;;  _ __ ___   __ _  __ _(_) |_
;; | '_ ` _ \ / _` |/ _` | | __|
;; | | | | | | (_| | (_| | | |_
;; |_| |_| |_|\__,_|\__, |_|\__|
;;                  |___/

(crafted-package-install-package 'magit)
(use-package magit
  :bind (("C-c g s" . 'magit-status)
         ("C-c G s" . 'magit-status)

         :map witek-context-key-map
         ("M"       . 'magit-file-dispatch)

         :map magit-status-mode-map
         ("x"       . 'magit-discard)))

;;     _                  _   _ _ _         _
;;  __| |___ __ _ _ _ ___| |_(_) | |___ _ _(_)_ _  __ _
;; / _| / -_) _` | ' \___| / / | | |___| '_| | ' \/ _` |
;; \__|_\___\__,_|_||_|  |_\_\_|_|_|   |_| |_|_||_\__, |
;;                                                |___/

;; (crafted-package-install-package 'clean-kill-ring)
;; (use-package clean-kill-ring
;;   ;; :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
;;   :config
;;   (clean-kill-ring-mode 1))

;;  _
;; | |_ _ _ ___ ___ _ __  __ _ __ ___
;; |  _| '_/ -_) -_) '  \/ _` / _(_-<
;;  \__|_| \___\___|_|_|_\__,_\__/__/

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

;;                    _
;;  ____ __  __ _ _ _| |_ _ __  __ _ _ _ __ _ _ _  ___
;; (_-< '  \/ _` | '_|  _| '_ \/ _` | '_/ _` | ' \(_-<
;; /__/_|_|_\__,_|_|  \__| .__/\__,_|_| \__,_|_||_/__/
;;                       |_|

(crafted-package-install-package 'smartparens)
(use-package smartparens
  :demand t
  :bind (("M-l" . 'sp-forward-slurp-sexp) ; owerride: downcase-word
         ("M-h" . 'sp-forward-barf-sexp)  ; override: mark-paragraph

         ("M-w" . 'sp-clone-sexp)         ; override: kill-ring-save
         ("H-w" . 'kill-ring-save)

         :map witek-context-key-map
         ("=" . 'sp-indent-defun)
         )

  :config
  (show-smartparens-global-mode 1)
  )

(require 'smartparens-config)

;;             _            _
;;  _ _ ___ __| |_ __ _ _ _| |_ ___ ___ _ __  __ _ __ ___
;; | '_/ -_|_-<  _/ _` | '_|  _|___/ -_) '  \/ _` / _(_-<
;; |_| \___/__/\__\__,_|_|  \__|   \___|_|_|_\__,_\__/__/

(crafted-package-install-package 'restart-emacs)
(use-package restart-emacs
  :bind (("C-c e r" . 'restart-emacs))
  )


;; __ __ ____ _ _ _ ___ _ __
;; \ V  V / _` | '_/ -_) '_ \
;;  \_/\_/\__, |_| \___| .__/
;;        |___/        |_|

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


;;  __ ___ ___  _
;; / _` \ V / || |
;; \__,_|\_/ \_, |
;;           |__/

;; https://github.com/abo-abo/avy

(use-package avy
  :ensure t
  :bind ("C-j" . avy-goto-char-timer))

;;          _                           _
;;  __ _ __| |___  __ ___ _ __  ___  __| |___
;; / _` / _` / _ \/ _|___| '  \/ _ \/ _` / -_)
;; \__,_\__,_\___/\__|   |_|_|_\___/\__,_\___|

(crafted-package-install-package 'adoc-mode)
(use-package adoc-mode
  )


;;     _      _                                _
;;  __| |___ (_)_  _ _ _ ___ ___ _ __  ___  __| |___
;; / _| / _ \| | || | '_/ -_)___| '  \/ _ \/ _` / -_)
;; \__|_\___// |\_,_|_| \___|   |_|_|_\___/\__,_\___|
;;         |__/

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  )

;;     _    _
;;  __(_)__| |___ _ _
;; / _| / _` / -_) '_|
;; \__|_\__,_\___|_|

(use-package cider
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-print-fn 'fipp)

  (define-key witek-context-key-map (kbd "e b") 'cider-eval-buffer)
  (define-key witek-context-key-map (kbd "e l") 'cider-eval-last-sexp)
  (define-key witek-context-key-map (kbd "e s") 'cider-eval-region)

  )

;;     _  _              __         _
;;  __| |(_)___ _ _ ___ / _|__ _ __| |_ ___ _ _
;; / _| || |___| '_/ -_)  _/ _` / _|  _/ _ \ '_|
;; \__|_|/ |   |_| \___|_| \__,_\__|\__\___/_|
;;     |__/

(use-package clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files t))


(setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically 't
   )

(setq cider-font-lock-reader-conditionals nil)
(setq cider-auto-inspect-after-eval t)
(setq cider-save-file-on-load t)


;;  ___ _ _ __ _
;; / _ \ '_/ _` |
;; \___/_| \__, |
;;         |___/


(setq org-directory "~/org/")

;; *** outshine - Org features in code files
;; [[https://github.com/alphapapa/outshine]]
;; [[https://orgmode.org/guide/Hyperlinks.html]]
;; (use-package outshine
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
;;   )
;; (global-origami-mode 1)

;;   __ _      _     _
;;  / _(_)__ _| |___| |_
;; |  _| / _` | / -_)  _|
;; |_| |_\__, |_\___|\__|
;;       |___/

(crafted-package-install-package 'figlet)
(use-package figlet
  )


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
