;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;; (setq-default left-fringe-width  16)
;; (setq-default right-fringe-width  16)
(set-frame-parameter nil 'internal-border-width 8)

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

;;; ef-themes

;; (crafted-package-install-package 'ef-themes)
(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-elea-light ef-elea-dark))
  (setq ef-themes-headings           ; read the manual's entry or the doc string
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4)
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (setq ef-themes-region '(intense no-extend neutral))
  (mapc #'disable-theme custom-enabled-themes)
  ;; (ef-themes-select 'ef-maris-dark)
  (ef-themes-select 'ef-deuteranopia-dark)
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
(set-frame-font "JetBrains Mono-12" nil t)
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
                            '(:font "JetBrains Mono" :height 110 :weight light)
                            ;; )))

;;; doom-modeline

(use-package doom-modeline
  :init
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 35)
  ;; (setq doom-modeline-hud t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-window-width-limit 20)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-always-visible-segments '())
  (doom-modeline-mode 1)
  )

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

;;; custom faces

(set-face-attribute 'mode-line nil :inherit 'variable-pitch)

(custom-set-faces
 ;; '(org-level-1 ((t (:inherit default :weight bold :foreground "gray80" :font "Source Sans Pro" :height 1.75))))
 ;; '(outline-minor-1 ((t (:inherit default :foreground "gray60" :font "Rubik Dirt" :height 2.0))))
 ;;
 )

;;; spacious-padding
(spacious-padding-mode 1)

;; TODO

;;; transparency

;; (set-frame-parameter nil 'alpha-background 90)

;;; provide

(provide 'my-theme)

