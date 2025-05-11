;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;; (setq-default left-fringe-width  16)
;; (setq-default right-fringe-width  16)
;; (set-frame-parameter nil 'internal-border-width 8)

;; (crafted-package-install-package 'spacegray-theme)
;; (use-package spacegray-theme
;;   )

;;; doom-themes

;; (crafted-package-install-package 'doom-themes)
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t
;;         doom-themes-padded-modeline t)

;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   (setq doom-themes-treemacs-theme "doom-atom")
;;   (doom-themes-treemacs-config)
;;   (doom-themes-org-config)

;;   (consult-theme 'doom-one)
;;   ;; (load-theme 'doom-one t)
;; )

;;; spacious-padding

(use-package spacious-padding
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

;;; ef-themes

;; (crafted-package-install-package 'ef-themes)
(use-package ef-themes
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

  ;; (defun my/set-theme (frame)
  ;;   (mapc #'disable-theme custom-enabled-themes)
  ;;   (ef-themes-select 'ef-owl))
  ;; (add-hook 'after-make-frame-functions 'my/set-theme)

  )

;;; pulsar
;; https://protesilaos.com/emacs/pulsar#h:96289426-8480-4ea6-9053-280348adc0ed

(use-package pulsar

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

;;; ligature

(use-package ligature
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

;;; fira-code-mode

;; (crafted-package-install-package 'fira-code-mode)
;; (use-package fira-code-mode
;;   :config
;;   (global-fira-code-mode)

;;   )

;;; set default font

;; (set-face-attribute 'default t :font "Fira Code-12" )

;; here
;; (set-frame-font "JetBrains Mono-12" nil t)
;; (setq default-frame-alist '((font . "JetBrains Mono-12")))
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))
;; (set-face-attribute 'default nil :height 120)

;; abcdefghijklmnopqrstuvwxyz 0123456789

;; (cond

   ;; ((> (x-display-pixel-height) 1080)
    ;; (customize-set-variable 'crafted-ui-default-font
                            ;; '(:font "Fira Code" :height 110)
                            ;; '(:font "JetBrains Mono" :height 110 :weight light)
                            ;; '(:font "Roboto Mono" :height 110)
                            ;; '(:font "Iosevka" :height 120)
                            ;; ))

   ;; (:else
    ;; (customize-set-variable 'crafted-ui-default-font
                            ;; '(:font "Fira Code" :height 110)
                            ;; '(:font "JetBrains Mono" :height 110 :weight light)
                            ;; )))


;; (set-face-attribute 'mode-line nil :inherit 'variable-pitch)

;;; doom-modeline

;; (use-package doom-modeline
;;   :init
;;   (setq doom-modeline-support-imenu t)
;;   (setq doom-modeline-height 35)
;;   ;; (setq doom-modeline-hud t)
;;   (setq doom-modeline-project-detection 'project)
;;   (setq doom-modeline-window-width-limit 20)
;;   (setq doom-modeline-buffer-file-name-style 'auto)
;;   (setq doom-modeline-minor-modes nil)
;;   (setq doom-modeline-buffer-encoding nil)
;;   (setq doom-modeline-indent-info t)
;;   (setq doom-modeline-lsp-icon t)
;;   (setq doom-modeline-always-visible-segments '())
;;   (doom-modeline-mode 1)
;;   )

;;; outline

(use-package outline
  :config
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'clojure-mode-hook #'outline-minor-mode))

;;; outline-minor-faces

;; (crafted-package-install-package 'outline-minor-faces)
(use-package outline-minor-faces
  :after outline

  :config
  (add-hook 'outline-minor-mode-hook
            #'outline-minor-faces-mode))



;;; transparency

;; (set-frame-parameter nil 'alpha-background 96)

;;; provide

(provide 'my-theme)

