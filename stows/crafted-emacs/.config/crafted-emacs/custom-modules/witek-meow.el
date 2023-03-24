;;; witek-meow.el --- MEOW specific configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <witek@helix>

(straight-use-package 'use-package)

(defun meow-setup ()

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))

  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
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
   '("?" . meow-cheatsheet))


  (meow-normal-define-key

   ;; Movement
   '("h" . meow-left)
   '("l" . meow-right)
   '("j" . meow-next)
   '("k" . meow-prev)

   ;; Movement + Navigation
   '("e" . meow-next-symbol)
   '("E" . meow-next-word)
   '("b" . meow-back-symbol)
   '("B" . meow-back-word)

   ;; Selection
   '("V" . meow-line)
   '("w" . meow-mark-symbol)
   '("W" . meow-mark-word)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("(" . meow-inner-of-thing)
   '(")" . meow-bounds-of-thing)
   '("f" . meow-find)
   '("t" . meow-till)
   '("m" . meow-join)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; these can be remapped, because cursor keys work the same
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)

   '("g" . meow-cancel-selection)

   ;; Editing
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("c" . meow-change)
   '("r" . meow-replace)

   ;; Deleting
   '("d" . meow-kill)
   '("D" . meow-kill-whole-line)
   '("x" . meow-delete)

   '("u" . meow-undo)
   '("U" . undo-redo)

   ;; Misc
   '("." . repeat)
   '("R" . meow-reverse)
   '("-" . negative-argument)
   '("v" . meow-visit)
   '("q" . meow-quit)

   '("y" . meow-save)
   '("p" . meow-yank)

   '("G" . meow-grab)
   '("_" . meow-swap-grab)
   '("Y" . meow-sync-grab)

   '("n" . meow-search)
   '("Q" . meow-goto-line)
   '("X" . meow-goto-line)

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
   '("<escape>" . ignore)))

(global-set-key (kbd "C-s") 'isearch-forward)


(use-package meow
  :straight t)

(require 'meow)
(meow-setup)
(meow-global-mode 1)

;; ---
(provide 'witek-meow)
