;; -*- lexical-binding: t; -*-

(defun meow-setup-keys ()

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
   '("%" . witek-matching-paren)

   ;; Selection
   '("V" . meow-line)
   '("w" . witek-meow-mark-symbol)
   '("W" . witek-meow-mark-word)
   '("b" . meow-block)
   '("O" . meow-to-block)
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
   '("M-j" . join-line)
   ;; '("<escape>" . ignore)

   '("y" . meow-save)
   '("p" . meow-yank)
   '("R" . meow-replace)
   '("P" . consult-yank-pop)

   '("G" . meow-grab)
   '("_" . meow-swap-grab)
   '("Y" . meow-sync-grab)

   '("n" . meow-search)

   '("=" . witek-indent-region-or-defun)

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
   '("C-h" . sp-beginning-of-sexp)
   '("C-l" . sp-end-of-sexp)

   '("," . my/activate-context-key-map)

   '("M" . magit-status)

   '("#" . clojure-toggle-ignore)

   '("B" . bookmark-set)

   '("g g" . beginning-of-buffer)
   '("g r" . xref-find-references)
   '("g i" . consult-imenu)
   '("g o" . consult-outline)
   '("g b" . consult-bookmark)

   '("m" . consult-register-store)
   '("'" . consult-register-load)

   '("@" . other-window)

   '("," . witek-context-key-map)
   ;;
   )

  )

(crafted-package-install-package 'meow)
(use-package meow
  :demand t

  :init

  (setq meow--kbd-kill-ring-save "H-w")

  (setq meow-cursor-type-normal '(bar . 4))

  ;; disable anoying hints when expanding
  (setq meow-expand-hint-counts ())

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
        meow-use-enhanced-selection-effect t)

  :config

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-setup-keys)
  (meow-setup-indicator)
  (meow-global-mode 1)

  (when (fboundp 'corfu-quit)
    (add-hook 'meow-insert-exit-hook 'corfu-quit))

  )

;; (require 'meow)


;; paren state
;; (setq meow-paren-keymap (make-keymap))
;; (meow-define-state paren
;;  "meow state for interacting with smartparens"
;;  :lighter " [P]"
;;  :keymap meow-paren-keymap)

;; meow-define-state creates the variable
;;(setq meow-cursor-type-paren 'hollow)

;;(meow-define-keys 'paren
;;  '("<escape>" . meow-normal-mode)
;;  '("l" . sp-forward-sexp)
;;  '("h" . sp-backward-sexp)
;;  '("j" . sp-down-sexp)
;;  '("k" . sp-up-sexp)
;;  '("n" . sp-forward-slurp-sexp)
;;  '("b" . sp-forward-barf-sexp)
;;  '("v" . sp-backward-barf-sexp)
;;  '("c" . sp-backward-slurp-sexp)
;;  '("u" . meow-undo))


;; ---
(provide 'witek-meow)
