;;; init.el --- Tangeled from emacs-config.org  -*- lexical-binding: t; -*-

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply
                     #'call-process
                     `("git" nil ,buffer t "clone"
                       ,@(when-let* ((depth (plist-get order :depth)))
                           (list (format "--depth=%d" depth) "--no-single-branch"))
                       ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process
                           emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq custom-file (make-temp-file "emacs-custom-"))

(setq make-backup-files nil)
(setq backup-inhibited nil)
(setq create-lockfiles nil)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

(use-package emacs
  :ensure nil
  :demand t

  :config

  (setq user-full-name "Witoslaw Koczewski")
  (setq user-mail-address "wi@koczewski.de")

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

;;; ** automatic saving and backups

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
  
)

(defvar witek-context-key-map (make-sparse-keymap) "My Context Keymap")
(defalias 'witek-context-key-map witek-context-key-map)

(defun my/set-context-key (kbd-string command-symbol)
  (define-key witek-context-key-map (kbd kbd-string) command-symbol))

(defun my/activate-context-key-map ()
  "Set `witek-context-key-map' as the current transient map. Also show which-key."
  (interactive)
  (set-transient-map witek-context-key-map))

(defun my/set-custom-key (kbd-string command-symbol)
  (global-set-key (kbd (concat "C-c " kbd-string)) command-symbol))

(defmacro comment (&rest body)
  "Determine what to do with BODY.

If BODY contains an unquoted plist of the form (:eval t) then
return BODY inside a `progn'.

Otherwise, do nothing with BODY and return nil, with no side
effects."
  (declare (indent defun))
  (let ((eval))
    (dolist (element body)
      (when-let* (((plistp element))
                  (key (car element))
                  ((eq key :eval))
                  (val (cadr element)))
        (setq eval val
              body (delq element body))))
    (when eval `(progn ,@body))))

(defun my/backspace-dwim ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'sp-delete-region)
    (sp-backward-delete-char))
  )

(defun my/wrap-round ()
  (interactive)
  (sp-wrap-round)
  (insert " ")
  (backward-char)
  (meow-insert))
(define-key witek-context-key-map (kbd "(") 'my/wrap-round)

(defun my/wrap-square ()
  (interactive)
  (sp-wrap-square))
(define-key witek-context-key-map (kbd "[") 'my/wrap-square)

(defun my/wrap-curly ()
  (interactive)
  (sp-wrap-curly))
(define-key witek-context-key-map (kbd "{") 'my/wrap-curly)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))

(use-package ef-themes
  :ensure t

  :config
  (setq ef-themes-to-toggle '(ef-eagle ef-owl))
  (setq ef-themes-headings
        '((0 variable-pitch light 1.9)
          ;; (1 variable-pitch light 1.8)
          ;; (2 variable-pitch regular 1.7)
          ;; (3 variable-pitch regular 1.6)
          ;; (4 variable-pitch regular 1.5)
          ;; (5 variable-pitch 1.4)
          ;; (6 variable-pitch 1.3)
          ;; (7 variable-pitch 1.2)
          ;; (t variable-pitch 1.1)
          ))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(intense no-extend neutral))
  (mapc #'disable-theme custom-enabled-themes)

  (ef-themes-select 'ef-owl)

  )

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (spacious-padding-mode 1)
  )

(use-package pulsar
  :ensure t

  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.10)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-cyan)
  (setq pulsar-highlight-face 'pulsar-yellow)
  
  (setq pulsar-resolve-pulse-function-aliases t)

  (add-to-list 'pulsar-pulse-functions 'meow-search)
  (add-to-list 'pulsar-pulse-functions 'phi-search)
  (add-to-list 'pulsar-pulse-functions 'phi-search-backward)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'end-of-defun)

  (add-to-list 'pulsar-pulse-region-functions 'yank)
  (add-to-list 'pulsar-pulse-region-functions 'consult-yank-pop)  
  
  (pulsar-global-mode 1)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)

  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

  )

(use-package ligature
  :ensure t
  :load-path "path-to-ligature-repo"
  
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            ;; (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ;; ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                            ;; (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            ;; "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            ;; "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="
                            ))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package meow
  :ensure t

  :config

  (my/set-custom-key "x" ctl-x-map)

  (my/set-custom-key "<SPC>" 'execute-extended-command)
  (my/set-custom-key ":" 'eval-expression)
  
  (defun my/meow-setup-keys ()
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     ;; '("<escape>" . ignore)

     )

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("<SPC>" . execute-extended-command)
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     )

    (meow-normal-define-key

     ;; Movement
     '("h" . meow-left)
     '("l" . meow-right)
     '("j" . meow-next)
     '("k" . meow-prev)

     ;; Movement + Navigation
     '(")" . my/after-end-of-sexp)
     '("(" . my/before-beginning-of-sexp)
     '("K" . sp-beginning-of-previous-sexp)
     '("J" . sp-beginning-of-next-sexp)
     '("L" . meow-next-symbol)
     '("H" . meow-back-symbol)
     '("E" . meow-next-word)
     ;; '("B" . meow-back-word)
     '("e" . meow-end-of-thing)
     '("%" . my/matching-paren)
     '("M-k" . beginning-of-defun)
     '("M-j" . end-of-defun)
     '("M-l" . forward-sexp)
     '("M-h" . backward-sexp)

     ;; Selection
     '("V" . meow-line)
     '("w" . witek-meow-mark-symbol)
     '("W" . witek-meow-mark-word)
     '("b" . meow-block)
     '("B" . meow-to-block)
     '("s" . meow-inner-of-thing)
     '("S" . meow-bounds-of-thing)
     '("F" . meow-find)
     '("t" . meow-till)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("v" . meow-right-expand)
     ;; '("H" . meow-left-expand)
     ;; '("J" . meow-next-expand)
     ;; '("K" . meow-prev-expand)

     '("<escape>" . meow-cancel-selection)

     ;; Editing
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("c" . meow-change)

     ;; Deleting
     '("d" . meow-kill)
     '("D" . meow-kill-whole-line)
     '("<deletechar>" . meow-delete)
     '("<del>". meow-backward-delete)
     '("C-<backspace>" . sp-raise-sexp)
     '("<backspace>" . my/backspace-dwim)
     '("x" . meow-delete)

     '("u" . meow-undo)
     '("U" . undo-redo)
     ;; '("C-r" . undo-redo)

     ;; Misc
     '("." . repeat)
     '("r" . meow-reverse)
     '("-" . negative-argument)
     '("/" . meow-visit)
     '("q" . meow-quit)
     ;; '("<escape>" . ignore)

     '("y" . meow-save)
     '("p" . yank)
     '("R" . meow-replace)
     '("P" . consult-yank-pop)

     '("G" . meow-grab)
     '("_" . meow-swap-grab)
     '("Y" . meow-sync-grab)

     '("n" . meow-search)

     '("=" . my/indent-region-or-defun)

     '(":" . consult-goto-line)

     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)

     ;; '("U" . meow-undo-in-selection)
     ;; '("s" . meow-line)
     '("z" . meow-pop-selection)

     ;; more...
     '(";" . comment-line)
     '("G" . end-of-buffer)
     '("0" . beginning-of-line-text)
     '("$" . end-of-line)

     '("ö" . my/append-after-end-of-sexp)

     '("," . my/activate-context-key-map)

     '("M" . magit-status)

     '("#" . clojure-toggle-ignore)

     '("g g" . beginning-of-buffer)
     '("g r" . xref-find-references)
     '("g i" . consult-imenu)
     '("g o" . consult-outline)
     '("g b" . consult-bookmark)

     '("m" . consult-register-store)
     '("g m" . consult-register-load)
     '("g M" . consult-register)
     '("'" . consult-register-load)

     '("@" . other-window)

     '("," . witek-context-key-map)
     ;;
     )

    )
  
  (setq meow-cursor-type-normal '(bar . 4))

  ;; disable anoying hints when expanding
  ;; (setq meow-expand-hint-counts ())

  (setq meow-expand-hint-remove-delay 3)

  ;; (setq meow-char-thing-table ((?r . round)
  ;;                              (?s . square)
  ;;                              (?c . curly)
  ;;                              (?g . string)
  ;;                              (?e . symbol)
  ;;                              (?w . window)
  ;;                              (?b . buffer)
  ;;                              (?p . paragraph)
  ;;                              (?l . line)
  ;;                              (?d . defun)
  ;;                              (?. . sentence)) )

  ;; don't insert anything when undefided key is used
  (setq meow-keypad-self-insert-undefined nil)

  ;; quicker pupup
  (setq meow-keypad-describe-delay 0.1)

  (setq meow-use-clipboard t)

  (setq meow-use-cursor-position-hack t
        meow-use-enhanced-selection-effect nil)

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (my/meow-setup-keys)
  (meow-setup-indicator)
  (meow-global-mode 1)

  (when (fboundp 'corfu-quit)
    (add-hook 'meow-insert-exit-hook 'corfu-quit))

  )

(my/set-custom-key "b d" 'kill-current-buffer)
(my/set-custom-key "b j" 'bookmark-jump)
(my/set-custom-key "b J" 'bookmark-jump-other-frame)
  
(my/set-custom-key "d K" 'describe-keymap)
(my/set-custom-key "d m" 'describe-mode)

(my/set-custom-key "e q" 'save-buffers-kill-terminal)
(my/set-custom-key "e Q" 'save-buffers-kill-emacs)
(my/set-custom-key "e e" 'eval-expression)
(my/set-custom-key "e l" 'eval-last-sexp)
(my/set-custom-key "e b" 'eval-buffer)
(my/set-custom-key "e f" 'eval-defun)
(my/set-custom-key "e r" 'restart-emacs)

(my/set-custom-key "f S" 'save-buffer)
(my/set-custom-key "f f" 'find-file)

(my/set-custom-key "w w" 'other-window)
(my/set-custom-key "w l" 'window-left)
(my/set-custom-key "w r" 'window-right)
(my/set-custom-key "w n" 'next-window-any-frame)
(my/set-custom-key "w p" 'previous-window-any-frame)
(my/set-custom-key "w /" 'split-window-horizontally)
(my/set-custom-key "w -" 'split-window-vertically)
(my/set-custom-key "w d" 'delete-window)
(my/set-custom-key "w m" 'delete-other-windows)

(my/set-custom-key "s q" 'query-replace)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)

  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25)
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil)
  (recentf-mode 1)
  )

(use-package outline
  
  :config

  (my/set-custom-key "o t" 'outline-toggle-children)
  (my/set-custom-key "o f" 'outline-hide-other)
  (my/set-custom-key "o a" 'outline-show-all)

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'clojure-mode-hook #'outline-minor-mode)
  )

(use-package outline-minor-faces
  :ensure t
  :after outline

  :config
  (add-hook 'outline-minor-mode-hook
            #'outline-minor-faces-mode))

(use-package project
  :demand t
  
  :config
  (my/set-custom-key "p p" 'project-switch-project)
  (my/set-custom-key "p f" 'project-find-file)

  )

(let ((custom-modules (expand-file-name "custom-modules" user-emacs-directory)))
  (when (file-directory-p custom-modules)
    (message "adding custom-modules to load-path: %s" custom-modules)
    (add-to-list 'load-path custom-modules)))

(require 'my-commands)
(require 'my-basics)
(require 'my-extras)
(require 'my-org)
(require 'my-ai)
(require 'my-lsp)
;; (require 'my-eglot)
;; (require 'my-email)

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (find-file "~/.dotfiles/emacs-config.org")))


