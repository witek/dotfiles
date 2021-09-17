;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; * .spacemacs

;; ** layers

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/backup-inhibited.modemycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(

     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)

     asciidoc

     ;; csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     (helm :variables
           helm-follow-mode-persistent t)

     ;; better-defaults

     (clojure :variables
              clojure-backend 'cider
              clojure-enable-linters 'clj-kondo
              cider-repl-display-help-banner nil
              cider-pprint-fn 'fipp
              ;; clojure-indent-style 'align-arguments
              ;; clojure-align-forms-automatically t
              clojure-toplevel-inside-comment-form t
              cider-result-overlay-position 'at-point
              cider-overlays-use-font-lock t
              cider-repl-buffer-size-limit 100
              cider-auto-test-mode nil
              ;; clojure-enable-clj-refactor t
              clojure-enable-fancify-symbols t
              nrepl-use-ssh-fallback-for-remote-hosts t
              )

     colors

     ;; dash

     emacs-lisp

     ;; evil-cleverparens

     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk t
          git-enable-github-support t
          git-gutter-use-fringe t)

     html

     javascript

     json

     ;; Language server protocol with minimal visual impact
     ;; https://practicalli.github.io/spacemacs/install-spacemacs/clojure-lsp/lsp-variables-reference.html
     ;; (lsp :variables
     ;;      lsp-enable-on-type-formatting nil
     ;;      lsp-enable-indentation nil
     ;;      lsp-enable-snippet t

     ;;      ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
     ;;      ;; subtle highlighting for doom-gruvbox-light theme defined in dotspacemacs/user-config
     ;;      lsp-enable-symbol-highlighting t

     ;;      lsp-modeline-diagnostics-enable t
     ;;      ;; lsp-modeline-diagnostics-scope :workspace

     ;;      ;; popup documentation boxes
     ;;      ;; lsp-ui-doc-enable nil          ;; disable all doc popups
     ;;      lsp-ui-doc-show-with-cursor nil   ;; doc popup for cursor
     ;;      ;; lsp-ui-doc-show-with-mouse t   ;; doc popup for mouse
     ;;      ;; lsp-ui-doc-delay 2                ;; delay in seconds for popup to display
     ;;      lsp-ui-doc-include-signature t    ;; include function signature
     ;;      ;; lsp-ui-doc-position 'at-point  ;; top bottom at-point
     ;;      lsp-ui-doc-alignment 'window      ;; frame window

     ;;      ;; code actions and diagnostics text as right-hand side of buffer
     ;;      lsp-ui-sideline-enable nil
     ;;      lsp-ui-sideline-show-code-actions nil
     ;;      ;; lsp-ui-sideline-delay 500

     ;;      ;; lsp-ui-sideline-show-diagnostics nil

     ;;      ;; reference count for functions (assume their maybe other lenses in future)
     ;;      lsp-lens-enable t

     ;;      ;; Efficient use of space in treemacs-lsp display
     ;;      treemacs-space-between-root-nodes nil

     ;;      ;; Optimization for large files
     ;;      lsp-file-watch-threshold 10000
     ;;      lsp-log-io nil)

     lua

     (markdown :variables
               markdown-live-preview-engine 'vmd)

     (org :variables
          org-want-todo-bindings t
          org-agenda-files '("~/Dropbox/org")
          org-default-notes-file '("~/Dropbox/org/inbox.org"
                                         "~/Dropbox/org/gtd.org")
          ;; setq org-startup-indented t
          org-reverse-note-order t
          org-agenda-ndays 7
          org-agenda-show-all-dates t
          org-deadline-warning-days 14
          org-agenda-skip-scheduled-if-done t
          org-agenda-start-on-weekday nil
          org-agenda-start-with-log-mode t
          org-agenda-use-time-grid t
          org-capture-templates
                '(
                  ("t" "TODO" entry (file+headline "~/Dropbox/org/inbox.org" "INBOX")
                   "** TODO [#C]  %?\n\n%i\n\nCreated On: %u\nCreated From: file:%F"
                   :prepend t
                   :empty-lines 1
                   :clock-in nil
                   :clock-resume nil
                   ))
                )

     outshine

    ;parinfer

     php

     ;; ranger

     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)

     shell-scripts


     (spacemacs-modeline :variables
                         doom-modeline-height 12
                         doom-modeline-major-mode-color-icon t
                         doom-modeline-buffer-file-name-style 'relative-to-project
                         doom-modeline-display-default-persp-name t
                         doom-modeline-minor-modes nil
                         doom-modeline-modal-icon nil)

     ;; spell-checking
     semantic

     smex

     syntax-checking

     themes-megapack

     theming

     (unicode-fonts :variables
                    unicode-fonts-enable-ligatures t
                    unicode-fonts-ligature-modes '(prog-mode))

     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     yaml

     )
   ;; version-control
   ;; typescript)

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(super-save)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

;; ** init

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (agenda . 10))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         modus-vivendi
                         modus-operandi
                         spacemacs-dark
                         spacemacs-light
                         doom-dracula
                         badwolf
                         gruvbox

                         ;; molokai
                         ;; flatland
                         ;; melancholy
                         ;; dakrone
                         ;; solarized-dark
                         ;; afternoon
                         ;; deeper-blue
                         ;; ample
                         ;; brin
                         ;; cyberpunk
                         ;; farmhouse-dark
                         ;; heroku
                         ;; hickey
                         ;; material
                         ;; misterioso
                         ;; reverse
                         ;; spacegray
                         ;; subatomic
                         ;; subatomic256
                         ;; monokai
                         ;; graham
                         ;; junio
                         ;; lush
                         ;; gruber-darker
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("Source Code Pro"
   ;;                             :size 13
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.0)
   ;; dotspacemacs-default-font '("Ubuntu Mono"
   ;;                             :size 15
   ;;                             :weight light
   ;;                             :width condensed
   ;;                             :powerline-scale 1.0)
   ;; dotspacemacs-default-font '("Roboto Mono"
   ;;                             :size 14
   ;;                             :weight normal
   ;;                             :powerline-scale 0.8)
   dotspacemacs-default-font '("Fira Code"
                               :size 14
                               ;; :weight light
                               ;; :width condensed
                               :powerline-scale 1.0
                               )

   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "C-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-,"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 60
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

;; ** user-init

;; *** modus themes

(defun witek/user-init-modus-themes ()
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-no-mixed-fonts nil
        modus-themes-subtle-line-numbers nil

        modus-themes-fringes 'intense ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers': nil,
        ;; 'straight-underline, 'subtle-foreground,
        ;; 'subtle-foreground-straight-underline, 'intense-foreground,
        ;; 'intense-foreground-straight-underline, 'colored-background
        modus-themes-lang-checkers 'intense-foreground-straight-underline

        ;; Options for `modus-themes-mode-line': nil, '3d, 'moody,
        ;; 'borderless, 'borderless-3d, 'borderless-moody, 'accented,
        ;; 'accented-3d, 'accented-moody
        modus-themes-mode-line 'accented

        ;; Options for `modus-themes-syntax': nil, 'faint,
        ;; 'yellow-comments, 'green-strings,
        ;; 'yellow-comments-green-strings, 'alt-syntax,
        ;; 'alt-syntax-yellow-comments, 'faint-yellow-comments
        modus-themes-syntax 'alt-syntax-yellow-comments

        ;; Options for `modus-themes-hl-line': nil, 'intense-background,
        ;; 'accented-background, 'underline-neutral,
        ;; 'underline-accented, 'underline-only-neutral,
        ;; 'underline-only-accented
        modus-themes-hl-line 'underline-accented

        modus-themes-paren-match 'intense-bold ; {nil,'subtle-bold,'intense,'intense-bold}

        ;; Options for `modus-themes-links': nil, 'faint,
        ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
        ;; 'underline-only, 'neutral-underline-only
        modus-themes-links 'neutral-underline

        ;; Options for `modus-themes-prompts': nil, 'subtle-accented,
        ;; 'intense-accented, 'subtle-gray, 'intense-gray
        modus-themes-prompts 'intense-accented

        modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}

        ;; Options for `modus-themes-region': nil, 'no-extend, 'bg-only,
        ;; 'bg-only-no-extend, 'accent, 'accent-no-extend
        modus-themes-region 'accent

        ;; Options for `modus-themes-diffs': nil, 'desaturated,
        ;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
        modus-themes-diffs 'fg-only-deuteranopia

        modus-themes-org-blocks 'rainbow ; {nil,'grayscale,'rainbow}
        modus-themes-org-habit nil ; {nil,'simplified,'traffic-light}

        modus-themes-headings ; this is an alist: read the manual or its doc string
        '((1 . line)
          (2 . rainbow-line-no-bold)
          (t . no-bold))

        modus-themes-variable-pitch-ui t
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t

        ;; modus-themes-scale-1 1.1
        ;; modus-themes-scale-2 1.15
        ;; modus-themes-scale-3 1.21
        ;; modus-themes-scale-4 1.27
        ;; modus-themes-scale-5 1.33

        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.28
        modus-themes-scale-5 1.36
        )
  )

;; *** dotspacemacs/user-init

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (witek/user-init-modus-themes)

  ;; custom theme modification
  ;; spacemacs - overriding default height of modeline
  ;; doom-gruvbox - subtle lsp symbol highlight
  (setq-default
   theming-modifications
   '((doom-dracula
      ;; (mode-line :height 0.92)
      ;; (mode-line-inactive :height 0.92)
      (lsp-face-highlight-read :background nil :weight bold)
      (command-log-command :foreground "firebrick")
      (command-log-key :foreground "dark magenta")
      )))


  (setq-default git-magit-status-fullscreen t))

;; ** user-config

;; *** theming

(defun witek/user-config-theming ()

  (defun practicalli/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'practicalli-modeline 'default))

  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-modeline 'practicalli-modeline
                                '(workspace-name window-number modals persp-name buffer-info matches remote-host vcs)
                                '(misc-info repl lsp))
    (practicalli/setup-custom-doom-modeline))

  )

;; *** global

(defun witek/user-config-global ()

  ;; improves performance, they said
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; zooming
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)
  (define-key global-map (kbd "C-0") 'spacemacs/reset-font-size)

  ;; History
  (setq history-delete-duplicates t)
  (setq extended-command-history
        (delq nil (delete-dups extended-command-history)))

  ;; browser
  (setq browse-url-browser-function 'browse-url-chrome)

  ;; disable minor mode indicators (which cause modeline crashes)
  (spaceline-toggle-minor-modes-off)

  ;; indent guide
  (spacemacs/toggle-indent-guide-globally-on)

  )



;; *** clojure

(defun witek/user-config-clojure ()

  ;; LSP
  ;; (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  ;; (setq lsp-modeline-diagnostics-scope :workspace)

  (defun witek/setup-clojure-mode (m)
    (spacemacs/declare-prefix-for-mode m "," "witek's")
    (spacemacs/set-leader-keys-for-major-mode m
      ",a" 'evil-cp-insert-at-end-of-form
      ",i" 'evil-cp-insert-at-beginning-of-form
      ;; "==" 'lsp-format-buffer
      "(" 'sp-wrap-round
      "#" 'cider-toggle-ignore-next-form
      ))
  (witek/setup-clojure-mode 'clojure-mode)
  (witek/setup-clojure-mode 'clojurescript-mode)
  (witek/setup-clojure-mode 'clojurec-mode)

  (defun witek/clojure-mode-hook ()
    (setq-local comment-column 0)

    (spacemacs/toggle-highlight-indentation-current-column-off)

    ;; kebab-case in clojure mode
    (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
    (modify-syntax-entry ?_ "w" clojure-mode-syntax-table)

    (spacemacs/toggle-truncate-lines-off)

    ;; indent guide
    (spacemacs/toggle-indent-guide-on)
    (spacemacs/toggle-indent-guide-globally-on)

    ;; column indicator
    (spacemacs/toggle-fill-column-indicator-on)

    )
  (add-hook 'clojure-mode-hook 'witek/clojure-mode-hook)
  (add-hook 'clojurescript-mode-hook 'witek/clojure-mode-hook)
  (add-hook 'clojurec-mode-hook 'witek/clojure-mode-hook)


  )
;; *** TEMPLATE
(defun witek/user-config-TEMPLATE ()

  )

;; *** user-config

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (witek/user-config-theming)
  (witek/user-config-global)


  ;;;
  ;;; TODO cleanup
  ;;;

  (setq scroll-conservatively 101
        scroll-margin 12
        scroll-preserve-screen-position 't)

  (setq magit-repository-directories '("/p/" "/home/witek/eclipse/default/")
        global-git-commit-mode t  )

  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        css-indent-offset 2)

  (setq x-select-enable-clipboard t)

  (setq

   ;; prevent auto-switch to lisp state
   evil-lisp-state-enter-lisp-state-on-command nil

   dotspacemacs-enable-paste-transient-state nil
   auto-completion-enable-help-tooltip t
   )

  (setq create-lockfiles nil)

  ;; auto save
  (setq auto-save-default nil
        super-save-auto-save-when-idle t
        super-save-mode +1)

  ;; Persist Undo tree
  ;; (setq undo-tree-auto-save-history t
  ;;       undo-tree-history-directory-alist `(("." . ,(concat spacemacs-cache-directory "undo"))))
  ;; (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
  ;;   (make-directory (concat spacemacs-cache-directory "undo")))

  ;; (setq default-frame-alist '((undecorated . t)))
  (setq frame-resize-pixelwise t)

  (spacemacs/toggle-transparency)

  ;; activate evil safe structural editing
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)

  (load "/home/witek/myfiles/misc.el")

  ;; Space Menu

  ;; (spacemacs/set-leader-keys ";" 'evilnc-comment-or-uncomment-lines)



  ;; toggle lisp state
  (evil-leader/set-key "." 'lisp-state-toggle-lisp-state)


  ;; jumping forward and backward
  (define-key evil-normal-state-map (kbd "M-<left>") 'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "M-<right>") 'evil-next-buffer)
  (define-key evil-normal-state-map (kbd "C-<left>") 'evil-jump-backward)
  (define-key evil-normal-state-map (kbd "C-<right>") 'evil-jump-forward)

  ;; moving in insert mode
  ;; disabled because it interfers with code completion suggestion selection
  ;; (define-key evil-insert-state-map (kbd "C-l") 'right-char)
  ;; (define-key evil-insert-state-map (kbd "C-h") 'left-char)
  ;; (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  ;; (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)

  ;; evil-easymotion
  (define-key evil-normal-state-map (kbd "C-j") 'evilem-motion-find-char)

  ;; scrolling
  ;; (define-key evil-normal-state-map (kbd "C-k") 'scroll-up)
  ;; (define-key evil-normal-state-map (kbd "C-j") 'scroll-down)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-cp-<)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-cp->)

  (define-key evil-normal-state-map (kbd ";") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd ";") 'evilnc-comment-or-uncomment-lines)

  ;; gotos
  (define-key evil-normal-state-map (kbd "g d") 'spacemacs/clj-find-var)
  ;; (define-key evil-normal-state-map (kbd "g d") 'evil-goto-definition)
  (define-key evil-normal-state-map (kbd "g v") 'cider-find-var)
  ;; (define-key evil-normal-state-map (kbd "g D") 'helm-imenu-in-all-buffers)
  (define-key evil-normal-state-map (kbd "g D") 'spacemacs/helm-jump-in-buffer)
  (define-key evil-normal-state-map (kbd "g f") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "g F") 'helm-recentf)
  (define-key evil-normal-state-map (kbd "g r") 'lsp-find-references)
  (define-key evil-normal-state-map (kbd "g t") 'projectile-toggle-between-implementation-and-test)
  ;; (define-key evil-normal-state-map (kbd "=") 'cider-format-defun)

  ;; symbol highlighting
  (spacemacs/toggle-automatic-symbol-highlight-on)


  ;; kebab-case in lisp mode
  (modify-syntax-entry ?- "w" lisp--mode-syntax-table)


  (witek/user-config-clojure)





  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

  ;;;
  ;;; org
  ;;;

  (with-eval-after-load 'org
    (spacemacs/set-leader-keys "oc" 'org-capture)
    (spacemacs/set-leader-keys "oa" 'org-agenda)

    (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
    )


  ;;;
  ;;; dired
  ;;;

 ;; (setq dired-listing-switches "--agho --group-direcotries-first")

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)

  )

;; ** customize

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xterm-color powerline shell-pop org-category-capture log4e gntp multi-term skewer-mode simple-httpd json-snatcher json-reformat parent-mode request haml-mode gitignore-mode flx highlight transient iedit anzu goto-chg eshell-z eshell-prompt-extras esh-help web-completion-data dash-functional tern hydra inflections edn multiple-cursors peg lv eval-sexp-fu sesman parseedn parseclj a bind-map bind-key packed auto-complete popup pos-tip pkg-info epl dash super-save phpunit phpcbf php-extras php-auto-yasnippets org-mime drupal-mode php-mode csv-mode company-quickhelp typescript-mode flycheck diminish cider seq clojure-mode paredit smartparens magit magit-popup git-commit with-editor f evil company helm helm-core yasnippet avy markdown-mode async alert projectile js2-mode s yaml-mode zeal-at-point ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tide tagedit stickyfunc-enhance srefactor spaceline smex smeargle slim-mode scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters pug-mode popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu emmet-mode elisp-slime-nav dumb-jump define-word company-web company-tern company-statistics column-enforce-mode color-identifiers-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-auto-select-error-buffer nil)
 '(cider-font-lock-dynamically (quote (macro function var deprecated core)))
 '(cider-font-lock-reader-conditionals nil)
 '(cider-mode-line-show-connection t)
 '(cider-offer-to-open-cljs-app-in-browser nil)
 '(cider-save-file-on-load t)
 '(cljr-hotload-dependencies t)
 '(cljr-warn-on-eval nil)
 '(custom-safe-themes
   (quote
    ("3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "0d75aa06198c4245ac2a8877bfc56503d5d8199cc85da2c65a6791b84afb9024" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default)))
 '(dired-listing-switches "-logahrt --group-directories-first --time-style=long-iso")
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (insert-shebang helm-gtags ggtags flycheck-bashate fish-mode counsel-gtags counsel swiper ivy company-shell xterm-color powerline shell-pop org-category-capture log4e gntp multi-term skewer-mode simple-httpd json-snatcher json-reformat parent-mode request haml-mode gitignore-mode flx highlight transient iedit anzu goto-chg eshell-z eshell-prompt-extras esh-help web-completion-data dash-functional tern hydra inflections edn multiple-cursors peg lv eval-sexp-fu sesman parseedn parseclj a bind-map bind-key packed auto-complete popup pos-tip pkg-info epl dash super-save phpunit phpcbf php-extras php-auto-yasnippets org-mime drupal-mode php-mode csv-mode company-quickhelp typescript-mode flycheck diminish cider seq clojure-mode paredit smartparens magit magit-popup git-commit with-editor f evil company helm helm-core yasnippet avy markdown-mode async alert projectile js2-mode s yaml-mode zeal-at-point ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tide tagedit stickyfunc-enhance srefactor spaceline smex smeargle slim-mode scss-mode sass-mode restart-emacs ranger rainbow-mode rainbow-identifiers rainbow-delimiters pug-mode popwin persp-mode pcre2el paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file neotree move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu emmet-mode elisp-slime-nav dumb-jump define-word company-web company-tern company-statistics column-enforce-mode color-identifiers-mode coffee-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(powerline-default-separator nil)
 '(safe-local-variable-values
   (quote
    ((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (cider-default-cljs-repl . shadow)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
