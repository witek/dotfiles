;; witek-defaults.el --- Witek's Defaults based on Crafted Defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(straight-use-package 'use-package)


(setq crafted-load-custom-file nil)

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))
(setq max-lisp-eval-depth 16000)
(setq max-specpdl-size 25000)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(setq cursor-in-non-selected-windows nil)

(setq x-stretch-cursor t)

(setq undo-limit 80000000)

(setq truncate-string-ellipsis "…")

(setq delete-by-moving-to-trash t)

(setq help-window-select t)

(setq initial-scratch-message "")


(setq recenter-positions '(5 top bottom))

(setq scroll-conservatively 101)

(customize-set-variable 'scroll-margin 35)

(setq scroll-preserve-screen-position t)

(setq sentence-end-double-space nil)

;; (setq uniquify-buffer-name-style 'complete)

(setq window-combination-resize t)

(global-display-fill-column-indicator-mode t)

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

(save-place-mode 1)

(setq create-lockfiles nil)

;; Default to an indentation size of 2 spaces
(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

(customize-set-variable 'fill-column 80)

;; *** Auto Save

(let ((witek-auto-save-directory (concat crafted-config-var-directory "witek-auto-save/")))
  (setq backup-directory-alist
        `((".*" . ,witek-auto-save-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,witek-auto-save-directory t))))

(setq auto-save-default t)

;; just use identifier at point
(setq xref-prompt-for-identifier nil)

;; (set-face-attribute 'doom-mode-line nil :box '(:width 0))

;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

;; *** consult

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)


(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)


(customize-set-variable 'project-vc-merge-submodules t)

(setq browse-url-browser-function 'browse-url-chrome)

;; *** my custom keymaps

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-f") 'isearch-forward)
;; (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "C-b") 'isearch-repeat-backward)

(global-set-key (kbd "C-c <SPC>") 'execute-extended-command)
(global-set-key (kbd "C-c <RET>") 'save-buffer)
(global-set-key (kbd "C-c x") ctl-x-map)

(global-set-key (kbd "C-c e q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-c e Q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c e e") 'eval-expression)
(global-set-key (kbd "C-c e l") 'eval-last-sexp)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e f") 'eval-defun)

(global-set-key (kbd "C-c b b") 'consult-buffer)
(global-set-key (kbd "C-c b d") 'kill-current-buffer)

(global-set-key (kbd "C-c f s") 'save-buffer)
(global-set-key (kbd "C-c f S") 'witek-save-all-buffers)
(global-set-key (kbd "C-c f f") 'find-file)
(global-set-key (kbd "C-c f r") 'consult-recent-file)

(global-set-key (kbd "C-c t s") 'smartparens-mode)
(global-set-key (kbd "C-c t S") 'smartparens-strict-mode)

(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p s") 'consult-git-grep)

(global-set-key (kbd "C-c w w") 'other-window)
(global-set-key (kbd "C-c w l") 'window-left)
(global-set-key (kbd "C-c w r") 'window-right)
(global-set-key (kbd "C-c w n") 'next-window-any-frame)
(global-set-key (kbd "C-c w p") 'previous-window-any-frame)
(global-set-key (kbd "C-c w /") 'split-window-horizontally)
(global-set-key (kbd "C-c w -") 'split-window-vertically)
(global-set-key (kbd "C-c w d") 'delete-window)

(global-set-key (kbd "C-c s q") 'query-replace)

;; (global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-s") 'isearch-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Context Keymap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar witek-context-key-map (make-sparse-keymap) "Witek's Context Keymap")
(defalias 'witek-context-key-map witek-context-key-map)

(defun witek-activate-context-key-map ()
  "Set 'witek-context-key-map as the current transient map. Also show which-key."
  (interactive)
  (set-transient-map witek-context-key-map)
  ;; (progn
  ;;   (when (fboundp 'which-key-show-keymap)
  ;;     (which-key-show-keymap 'witek-context-key-map))
  ;;   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export witek-defaults ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'witek-defaults)
