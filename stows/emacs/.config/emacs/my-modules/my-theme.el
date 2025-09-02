;; -*- lexical-binding: t; -*-


;;; Font

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

;; https://protesilaos.com/emacs/pulsar#h:96289426-8480-4ea6-9053-280348adc0ed
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


;;; provide

(provide 'my-theme)
