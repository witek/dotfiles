;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;;; C-c custom keys (used as leader)

(my/set-custom-key "x" ctl-x-map)

(my/set-custom-key "<SPC>" 'execute-extended-command)
(my/set-custom-key ":" 'eval-expression)
;; (my/set-custom-key "<RET>" 'save-buffer)

(my/set-custom-key "d K" 'describe-keymap)
(my/set-custom-key "d m" 'describe-mode)

(my/set-custom-key "e q" 'save-buffers-kill-terminal)
(my/set-custom-key "e Q" 'save-buffers-kill-emacs)
(my/set-custom-key "e e" 'eval-expression)
(my/set-custom-key "e l" 'eval-last-sexp)
(my/set-custom-key "e b" 'eval-buffer)
(my/set-custom-key "e f" 'eval-defun)
(my/set-custom-key "e r" 'restart-emacs)

(my/set-custom-key "b b" 'consult-buffer)
(my/set-custom-key "b d" 'kill-current-buffer)
(my/set-custom-key "b j" 'bookmark-jump)
(my/set-custom-key "b J" 'bookmark-jump-other-frame)

(my/set-custom-key "f S" 'save-buffer)
(my/set-custom-key "f s" 'witek-save-all-buffers)
(my/set-custom-key "f f" 'find-file)
(my/set-custom-key "f r" 'consult-recent-file)

(my/set-custom-key "t s" 'smartparens-strict-mode)

(my/set-custom-key "p p" 'project-switch-project)
(my/set-custom-key "p f" 'project-find-file)
(my/set-custom-key "p b" 'consult-project-buffer)
(my/set-custom-key "p B" 'witek-project-kill-other-buffers)
(my/set-custom-key "p s" 'consult-git-grep)

(my/set-custom-key "w w" 'other-window)
(my/set-custom-key "w l" 'window-left)
(my/set-custom-key "w r" 'window-right)
(my/set-custom-key "w n" 'next-window-any-frame)
(my/set-custom-key "w p" 'previous-window-any-frame)
(my/set-custom-key "w /" 'split-window-horizontally)
(my/set-custom-key "w -" 'split-window-vertically)
(my/set-custom-key "w d" 'delete-window)

(my/set-custom-key "s q" 'query-replace)

(my/set-custom-key "o t" 'outline-toggle-children)
(my/set-custom-key "o f" 'outline-hide-other)
(my/set-custom-key "o a" 'outline-show-all)


;;; cusotm context key-map

(my/set-context-key "e b" 'eval-buffer)
(my/set-context-key "e l" 'eval-last-sexp)
(my/set-context-key "e s" 'eval-region)

(my/set-context-key "b s" 'bookmark-set)

;;; provide
(provide 'my-keys)
