;; * Witek's Emacs Config
;; Based on Crafted Emacs and davivil's config.

;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

;; ** Defaults

(require 'crafted-defaults)    ; Sensible default settings for Emacs
;(require 'crafted-compile)     ; Set up automatic compilation for some emacs-lisp files

;; Prevent loading custom.el
(setq crafted-load-custom-file nil)

;; *** TODO Start Emacs server (for emacsclient)
;;(server-start)

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(save-place-mode 1)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(setq cursor-in-non-selected-windows nil)

(setq x-stretch-cursor t)

(setq undo-limit 80000000)

(setq auto-save-default t)

(setq truncate-string-ellipsis "…")

(setq delete-by-moving-to-trash t)

(setq help-window-select t)

(setq initial-scratch-message "")

(setq read-process-output-max (* 1024 1024))

(setq recenter-positions '(5 top bottom))

(setq scroll-conservatively 101)

(setq scroll-margin 35)

(setq scroll-preserve-screen-position t)

(setq sentence-end-double-space nil)

(setq uniquify-buffer-name-style 'complete)

(setq window-combination-resize t)

(global-display-fill-column-indicator-mode t)

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

;; *** Auto Save

(let ((witek-auto-save-directory (concat crafted-config-var-directory "witek-auto-save/")))
  (setq backup-directory-alist
        `((".*" . ,witek-auto-save-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,witek-auto-save-directory t))))


;; *** Let's be Evil

(require 'crafted-evil)

;; Use C-h as usual
(customize-set-variable 'evil-want-C-u-delete nil)
(customize-set-variable 'evil-want-C-u-scroll t)
(customize-set-variable 'evil-want-C-i-jump t)
(customize-set-variable 'evil-want-C-d-scroll t)
(customize-set-variable 'evil-want-Y-yank-to-eol nil) ; does not work
(customize-set-variable 'evil-regexp-search nil) ; does not work
(customize-set-variable 'evil-cross-lines t)
;; alskdjf löaskdjf ölaksjd flökasjdflöksj dflkjsd alskjdf laskdjf aösldk aslködjf aslkdjf alskdjf

;; ** UI

(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-speedbar)    ; built-in file-tree

;; *** windows

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

;; *** consult

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; *** which-key
(crafted-package-install-package 'which-key)
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; *** Theme
;; also loaded in early-config.el
(crafted-package-install-package 'spacegray-theme)
(crafted-package-install-package 'doom-themes)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-themes-padded-modeline t
      )
(load-theme 'spacegray t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(setq doom-themes-treemacs-theme "doom-atom")
(doom-themes-treemacs-config)
(doom-themes-org-config)

;; *** Font

(crafted-package-install-package 'fira-code-mode)
;; (require 'fira-code-mode)
(global-fira-code-mode)
;; (fira-code-mode)
;; (add-hook 'prog-mode-hook 'fira-code-mode )
;; ->


(cond

 ((> (x-display-pixel-height) 1600)
  (customize-set-variable 'crafted-ui-default-font
                          '(:font "Fira Code" :height 110)))

 (:else
  (customize-set-variable 'crafted-ui-default-font
                          '(:font "Fira Code" :height 110))
  ))


;; *** general - Leader Keybindings
;; general.el for prefixed key bindings
;; https://github.com/noctuid/general.el
(crafted-package-install-package 'general)
(require 'general)
(general-evil-setup)

;; **** Leader

(general-create-definer my-leader-def
  :states (list 'normal 'visual)
  :prefix "SPC")

(my-leader-def
  "SPC" 'execute-extended-command
  "/" 'consult-git-grep

  "e q" 'save-buffers-kill-terminal
  "e Q" 'save-buffers-kill-emacs
  "e e" 'eval-expression
  "e f" 'make-frame

  "q q" 'save-buffers-kill-terminal
  "q Q" 'save-buffers-kill-emacs

  "b b" 'consult-buffer
  "b d" 'kill-current-buffer

  "f s" 'save-buffer
  "f S" 'save-some-buffers
  "f f" 'find-file
  "f r" 'consult-recent-file

  "w d" 'delete-window
  "w w" 'other-window
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w h" 'evil-window-left
  "w l" 'evil-window-right

  "d k" 'describe-key
  "d c" 'describe-command
  "d v" 'describe-variable
  "d f" 'describe-function
  "d m" 'describe-mode
  "d K" 'describe-keymap

  "t s" 'smartparens-mode
  "t S" 'smartparens-strict-mode

  )

(general-define-key
 :keymaps 'vertico-map
 :prefix "C-,"
 "e" 'embark-export
 "a" 'embark-act)

;; **** Local Leader

(general-create-definer my-local-leader-def
  :states (list 'normal 'visual)
  :prefix ",")

(my-local-leader-def
  :keymaps (list 'with-editor-mode-map)
  "," 'with-editor-finish
  "q" 'with-editor-cancel
  )

;; *** Restart Emacs

(crafted-package-install-package 'restart-emacs)
(require 'restart-emacs)
(my-leader-def
  "q r" 'restart-emacs
  "e r" 'restart-emacs)


;; ** project

(require 'crafted-project)

(customize-set-variable 'project-vc-merge-submodules t)

;; project-find-functions
;; (project-try-vc "/p/clj-crafted/kunagi-utils")

(my-leader-def
  "p p" 'project-switch-project
  "p f" 'project-find-file
  "p /" 'consult-git-grep)

;; ** magit - git Version Control

(crafted-package-install-package 'magit)

(my-leader-def
  "g s" 'magit-status)



;; ** Editing

(require 'crafted-editing)     ; Whitespace trimming, auto parens etc.

;; Default to an indentation size of 2 spaces
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

;; *** Margins

(customize-set-variable 'fill-column 80)

;; (crafted-package-install-package 'visual-fill-column)
;; (require 'visual-fill-column)
;; (custom-set-variables
;;  '(global-visual-fill-column-mode t)
;;  '(visual-fill-column-width 80))

;; *** Commenting / Uncommenting

(general-define-key
 :states '(normal visual)
 ";" 'evilnc-comment-or-uncomment-lines)

;; *** smartparens - Structural Editing
;; [[https://github.com/Fuco1/smartparens]]
;; [[https://github.com/expez/evil-smartparens]]

(crafted-package-install-package 'evil-smartparens)

(require 'smartparens-config)

;; (smartparens-global-strict-mode)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; *** cleverparens

(crafted-package-install-package 'evil-cleverparens)

(add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)



(general-define-key
 :keymaps 'evil-cleverparens-mode-map
 :states 'normal
 "M-l" 'evil-cp->
 "M-h" 'evil-cp-<)

;; (general-define-key
;;  :states '(normal visual)
;;  "c-l" 'evil-cp->
;;  "c-h" 'evil-cp-<
;;  )

;; *** symex - Structural Editing
;; [[https://github.com/countvajhula/symex.el]]

;; (crafted-package-install-package 'symex)
;; (symex-initialize)

;; *** Keybindings

(my-local-leader-def
  :keymaps (list 'emacs-lisp-mode-map 'clojure-mode-map)
  "(" 'sp-wrap-round
  "[" 'sp-wrap-square
  "{" 'sp-wrap-curly
  "=" 'indent-sexp
  ", i" 'evil-cp-insert-at-beginning-of-form
  ", a" 'evil-cp-insert-at-end-of-form
  )

;; ** eglot - IDE

(require 'crafted-ide)
(require 'eglot)

;; ** lsp-mode - IDE

;; (crafted-package-install-package 'lsp-mode)

;; ** Lisp

(require 'crafted-lisp)


;; ** Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)

(my-local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e v" 'eval-last-sexp)

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

;; *** Keybindings

(my-local-leader-def
  :keymaps 'clojure-mode-map
  "e v" #'cider-eval-sexp-at-point
  )

;; *** eglot

(add-to-list 'eglot-server-programs `(clojurescript-mode . ("clojure-lsp")))

;; ;; (add-to-list 'eglot-server-programs `(clojure-mode . ("clojure-lsp")))
;; ;; (add-to-list 'eglot-server-programs `(clojurescript-mode . ("clojure-lsp")))
;; ;; (add-to-list 'eglot-server-programs `(clojurec-mode . ("clojure-lsp")))

(add-hook 'clojure-mode-hook #'eglot-ensure)

;; ;; *** /p/clj/ as eglot root

;; ;; **** TODO use defadvice
(defun eglot--current-project ()
  "Witek's custom impl. Always /p/clj/"
  (message "eglot--current-project: witek's hack returns /p/clj/")
  (project-current nil "/p/clj/")
  )

;; ** Org

(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.

(setq org-directory "~/org/")

;; *** outshine - Org features in code files
;; [[https://github.com/alphapapa/outshine]]
;; [[https://orgmode.org/guide/Hyperlinks.html]]
(crafted-package-install-package 'outshine)

(defun witek-activate-outshine ()
  (outshine-mode 1)
  (general-define-key
   :keymaps 'outline-mode-map
   :states 'normal
   "M-l" 'nil
   "M-h" 'nil))

(add-hook 'emacs-lisp-mode-hook 'witek-activate-outshine)
(add-hook 'clojure-mode-hook 'witek-activate-outshine)

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
  "f d" 'witek-delete-current-file)
