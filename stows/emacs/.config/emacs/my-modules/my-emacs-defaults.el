;; -*- lexical-binding: t; -*-

;;; Disable backups and lockfiles

(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)


;;; Silence native compilation

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))



;;; Buffers

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; pop up dedicated buffers in a different window.
(setq switch-to-buffer-in-dedicated-window 'pop)
;; treat manual buffer switching (C-x b for example) the same as
;; programmatic buffer switching.
(setq switch-to-buffer-obey-display-actions t)

;; prefer the more full-featured built-in ibuffer for managing
;; buffers.
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
;; turn off forward and backward movement cycling
(setq ibuffer-movement-cycle nil)
;; the number of hours before a buffer is considered "old" by
;; ibuffer.
(setq ibuffer-old-time 24)

(setq undo-limit 1600000)

(setq truncate-string-ellipsis "…")

(setq-default truncate-lines nil)
(setq truncate-lines nil)

;; (global-display-fill-column-indicator-mode t)

(customize-set-variable 'fill-column 80)

(setq recenter-positions '(5 top bottom))

(setq global-visual-line-mode t)


;;; Completion settings

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)
(setq completion-category-overrides
      '((file (styles . (partial-completion)))))
(setq completions-detailed t)


;;; Editing

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; (setq-default tab-width 2)

;; Do not save duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(setq sentence-end-double-space nil)

(setq next-line-add-newlines nil)


;;; Persistence between sessions


;; Enable savehist-mode for command history
(savehist-mode 1)

;; save the bookmarks file every time a bookmark is made or deleted
;; rather than waiting for Emacs to be killed.  Useful especially when
;; Emacs is a long running process.
(setq bookmark-save-flag 1)


;;; Window management

(winner-mode 1)

;; (define-prefix-command 'crafted-windows-key-map)

;; (keymap-set 'crafted-windows-key-map "u" 'winner-undo)
;; (keymap-set 'crafted-windows-key-map "r" 'winner-redo)
;; (keymap-set 'crafted-windows-key-map "n" 'windmove-down)
;; (keymap-set 'crafted-windows-key-map "p" 'windmove-up)
;; (keymap-set 'crafted-windows-key-map "b" 'windmove-left)
;; (keymap-set 'crafted-windows-key-map "f" 'windmove-right)

;; (keymap-global-set crafted-windows-prefix-key 'crafted-windows-key-map)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 10)
(setq scroll-preserve-screen-position t)

;; open man pages in their own window, and switch to that window to
;; facilitate reading and closing the man page.
(setq Man-notify-method 'aggressive)

;; keep the Ediff control panel in the same frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))


;;; Dired

;; Make dired do something intelligent when two directories are shown
;; in separate dired buffers.
(setq dired-dwim-target t)

;; automatically update dired buffers on revisiting their directory
(setq dired-auto-revert-buffer t)


;;; Eshell

;; scroll eshell buffer to the bottom on input, but only in "this"
;; window.
(setq eshell-scroll-to-bottom-on-input 'this)


;;; Miscellaneous

;; Load source (.el) or the compiled (.elc or .eln) file whichever is
;; newest
(setq load-prefer-newer t)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Turn on repeat mode to allow certain keys to repeat on the last
;; keystroke. For example, C-x [ to page backward, after pressing this
;; keystroke once, pressing repeated [ keys will continue paging
;; backward. `repeat-mode' is exited with the normal C-g, by movement
;; keys, typing, or pressing ESC three times.
(unless (version< emacs-version "28")
  (repeat-mode 1))

(setq ad-redefinition-action 'accept)

(setq cursor-in-non-selected-windows nil)

(setq x-stretch-cursor t)


(setq help-window-select t)

(setq initial-scratch-message "")

(setq-default enable-local-variables t)

(setq confirm-kill-emacs nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;;; automatic saving and backups

(let ((my/auto-save-directory (expand-file-name "auto-save/" user-emacs-directory)))
  (setq backup-directory-alist
        `((".*" . ,my/auto-save-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,my/auto-save-directory t))))

;; Activate auto saving in every buffer
(setq auto-save-default t)

(save-place-mode 1)

(setq create-lockfiles nil)

(setq delete-by-moving-to-trash t)

;;; navigation

;; just use identifier at point
(setq xref-prompt-for-identifier nil)

(setq xref-auto-jump-to-first-xref 'show)

;;; files and directories

(setq vc-follow-symlinks t)

(customize-set-variable 'project-vc-merge-submodules t)

;;; external tools

;; (setq browse-url-browser-function 'browse-url-firefox)
(setq browse-url-browser-function 'browse-url-chrome)


;;; provide

(provide 'my-emacs-defaults)
