;; -*- lexical-binding: t; -*-


;;; witek-context-key-map

(defvar witek-context-key-map (make-sparse-keymap) "My Context Keymap")
(defalias 'witek-context-key-map witek-context-key-map)

(defun my/set-context-key (kbd-string command-symbol)
  (define-key witek-context-key-map (kbd kbd-string) command-symbol))

(defun my/activate-context-key-map ()
  "Set `witek-context-key-map' as the current transient map. Also show which-key."
  (interactive)
  (set-transient-map witek-context-key-map))

;;; my/set-custom-key

(defun my/set-custom-key (kbd-string command-symbol)
  (global-set-key (kbd (concat "C-c " kbd-string)) command-symbol))

;;; comment macro

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

;;; my/backspace-dwim

(defun my/backspace-dwim ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'sp-delete-region)
    (sp-backward-delete-char))
  )

;;; wrap-round wrap-square wrap-curly

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

;;; provide

(provide 'my-utils)
