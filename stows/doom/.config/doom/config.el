;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;           DO NOT MODIFY!
;;       tangeled from config.org

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Witoslaw Koczewski"
      user-mail-address "wi@koczewski.de")

(setq doom-localleader-key ",")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(print "[config.org] Auto-customizations")

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default
 ;; ad-redefinition-action 'accept         ; Silence warnings for redefinition
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 cursor-type '(hbar . 2)                ; Underline-shaped cursor
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 delete-by-moving-to-trash t            ; Delete files to trash
 fill-column 80                         ; Set width for automatic line breaks
 gc-cons-threshold (* 8 1024 1024)      ; We're not using Game Boys anymore
 help-window-select t                   ; Focus new help windows when opened
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-screen t               ; Disable start-up screen
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 read-process-output-max (* 1024 1024)  ; Increase read size per process
 recenter-positions '(5 top bottom)     ; Set re-centering positions
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 12                       ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 scroll-preserve-screen-position 't
 sentence-end-double-space nil          ; Use a single space after dots
 ;; show-help-function nil                 ; Disable help text everywhere
 tab-always-indent 'complete            ; Tab indents first then tries completions
 tab-width 2                            ; Smaller width for tab characters
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 ;; warning-minimum-level :error           ; Skip warning buffers
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t                    ; Stretch cursor to the glyph width

 )

;; (blink-cursor-mode 0)                   ; Prefer a still cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)           ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable downcase-region
(put 'upcase-region 'disabled nil)      ; Enable upcase-region
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(global-display-fill-column-indicator-mode t)

;; Change a few indenting behaviors.
(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)

(setq-default enable-local-variables t)

(setq display-line-numbers-type nil)

(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-dracula)

(print (> (x-display-pixel-height) 1600))

(cond

 ((> (x-display-pixel-height) 1600)
  (setq doom-font (font-spec :family "Fira Code" :size 28)
        doom-variable-pitch-font (font-spec :family "Ubuntu" :size 30)
        ivy-posframe-font (font-spec :family "Fira Code" :size 34)))

 (t
  (setq doom-font (font-spec :family "Fira Code" :size 14)
          doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
          ivy-posframe-font (font-spec :family "Fira Code" :size 17))))

(setq confirm-kill-emacs nil)

(add-hook 'smartparens-enabled-hook #'smartparens-strict-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)

(map! :map global-map
      :mode evil-cleverparens-mode
      :n "M-l" #'evil-cp->)
(map! :map global-map
      :mode evil-cleverparens-mode
      :n "M-h" #'evil-cp-<)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(map! :leader
      :desc "List commands"
      "SPC"  #'execute-extended-command)

(map! :map global-map
      :n "C-h" #'evil-prev-buffer
      :n "C-l" #'evil-next-buffer
      :n "C-j" #'evil-jump-forward
      :n "C-k" #'evil-jump-backward
      )

(map! :map global-map
      :nv ";" #'evilnc-comment-or-uncomment-lines)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(setq projectile-project-root-files-functions '(projectile-root-local
                                                projectile-root-top-down
                                                projectile-root-top-down-recurring
                                                projectile-root-bottom-up))
;; (setq projectile-project-root-files-functions '(projectile-root-local
;;                                                 projectile-root-top-down
;;                                                 projectile-root-top-down-recurring
;;                                                 projectile-root-bottom-up))

(print "[config.org] Git")

(map! :localleader
      :mode git-commit-mode
      :n :desc "Commit" "," #'with-editor-finish
      :n :desc "Quit commit" "q" #'with-editor-cancel)

(map! :leader
      (:prefix-map ("g" . "git")
       :desc "Magit status" "s" #'magit-status
       :desc "Magit status here" "S"   #'magit-status-here
       :desc "Git stage hunk" "g"   #'git-gutter:stage-hunk
       :desc "Git stage file" "G"   #'magit-stage-file
      ))

(setq org-directory "~/org/")

(map! :after org
      :map org-mode-map
      :localleader
      "e" nil
      (:prefix-map ("e" . "edit / eval / export")
       "e" #'eval-last-sexp
       "E" #'org-export-dispatch
       "s" #'org-edit-special
       ))

;; none of these works :-(
(map! :after org
      :map org-src-mode-map
      :localleader
      "," #'org-edit-src-exit)
;; (define-key org-src-mode-map (kbd ", ,") #'org-edit-src-exit)

(print "[config.org] LSP")

(use-package! lsp-ui
  :config

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\firebase\\'")

  (setq
   lsp-ui-imenu-enable t

   lsp-ui-doc-enable nil
   lsp-ui-doc-show-with-cursor t
   lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'bottom
   ;; lsp-ui-doc-delay 3

   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-code-actions nil

   lsp-lens-enable t

   lsp-enable-symbol-highlighting t
   lsp-enable-on-type-formatting t
   lsp-enable-indentation t
   lsp-enable-snippet t

   lsp-modeline-diagnostics-enable t
   lsp-file-watch-threshold 10000
   lsp-log-io nil
   )

  (map! :localleader
        :mode lsp-mode
        :n "=" #'lsp-format-buffer)
  )

(after! lisp-mode
  (modify-syntax-entry ?- "w" lisp-mode-syntax-table))


;; (map! :localleader
      ;; ",a"  #'evil-cp-insert-at-end-of-form
      ;; ",i" 'evil-cp-insert-at-beginning-of-form
      ;; "(" #'sp-wrap-round)

(print "[config.org] Clojure")

(use-package! clojure-mode
  :config
  (setq
   clojure-ident-style 'align-arguments
   clojure-align-forms-automatically 't
   )
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  )

(use-package! cider
  :config
  (map! :localleader
        :map (clojure-mode-map clojurescript-mode-map)
        "ev" #'cider-eval-sexp-at-point
        ",a" 'evil-cp-insert-at-end-of-form
        ",i" 'evil-cp-insert-at-beginning-of-form
        ;; "==" 'lsp-format-buffer
        ;; "(" 'sp-wrap-round
        ))
