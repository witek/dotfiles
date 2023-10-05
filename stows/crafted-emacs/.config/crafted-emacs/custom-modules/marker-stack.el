;;; marker-stack.el --- Buffer-local history of point locations  -*- lexical-binding: t; -*-

;; Copyright (c) 2010 Matt Harrison
;; Copyright (c) 2011-2014 Dmitry Gutov
;; Copyright (c) 2020-2023 Andrea Greselin

;; Authors: Matt Harrison <matthewharrison@gmail.com>
;;          Dmitry Gutov <dgutov@yandex.ru>
;;          Andrea Greselin <greselin.andrea@gmail.com>
;; Version: 0.9999

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This library provides backward and forward stacks of point
;; locations for each buffer. Each stack element is a marker position.
;; The stacks are buffer-local, so the commands for navigating the
;; history never switch buffers.
;;
;; To navigate the stacks, bind the navigation commands to some keys:
;;   (global-set-key (kbd "C-M-,") 'marker-stack-backward-stack-pop)
;;   (global-set-key (kbd "C-M-.") 'marker-stack-forward-stack-pop)
;;
;; To push values to the backward stack manually, bind the push
;; command:
;;   (global-set-key [f5] 'marker-stack-push)
;;
;; The recommended usage is to save locations automatically:
;;   (marker-stack-setup-advices)
;;
;; Advised functions save the point location at the time of their
;; invocation. You can choose which functions to advise by customizing
;; the variables ‘marker-stack-functions-every-call’ and
;; ‘marker-stack-functions-first-call’:
;;   (setq marker-stack-functions-every-call
;;         (remove 'replace-match-maybe-edit
;;                 marker-stack-functions-every-call))
;;   (add-to-list 'marker-stack-functions-every-call 'replace-search)
;; The difference between these two variables is that the functions in
;; ‘marker-stack-functions-every-call’ save the point location at
;; every invocation while those in ‘marker-stack-functions-first-call’
;; only save it at their first invocation, when called repeatedly.
;;
;; Based on https://github.com/dgutov/point-stack
;; which in turn is based on https://github.com/mattharrison/point-stack
;; which in turn is based on http://www.emacswiki.org/emacs/JohnConnors
;;
;; This library was forked from point-stack v. 1.1 (MELPA package
;; version 20170808.1658),
;; https://github.com/dgutov/point-stack/blob/76e17311e3a810314c7d31ac46dc55450ff30fa2/point-stack.el
;;
;; The reasons why I started with point-stack are:
;;  - it can restore the point’s last position, after navingating the
;;    history backwards, without need for the user to save it
;;    beforehand (no need to remember to ‘C-SPC C-SPC’);
;;  - it allows navigating the history both backwards and forwards --
;;    though there are ways to move forwards in the ‘mark-ring’ too:
;;    see Ian Kelling’s answer on
;;    https://stackoverflow.com/questions/3393834/how-to-move-forward-and-backward-in-emacs-mark-ring/14539202#14539202
;;  - its history is unrelated to the ‘mark-ring’, which has other
;;    uses in addition to providing a history of positions;
;;  - it provides a way to choose which functions add to the history
;;    (setting ‘point-stack-advised-functions’).
;;
;; The main differences with respect to point-stack are:
;;  - the history is local to each buffer. This means that
;;    marker-stack never switches buffer.
;;  - marker-stack stores the position information as markers instead
;;    of numbers. Markers keep track of the location relative to the
;;    surrounding text, while numbers record fixed positions with
;;    respect to the beginning of the buffer.
;;  - marker-stack’s stacks have a limited length.
;;  - In marker-stack making a new step moves the the forward stack
;;    to the backward stack instead of deleting it.
;;  - marker-stack doesn’t store the scroll position.
;;  - In point-stack there isn’t a way of saving the position
;;    automatically on just the first of many repeated invocations.
;;
;; TODO: Add an option to save the history to a file.

;;; Code:

(defgroup marker-stack nil
  "Buffer-local history of point locations."
  :group 'convenience)

;; A large number of markers can slow down Emacs (because it has to
;; keep track of them, unlike with number positions), so it’s better
;; to set a limit to the length of the stacks.
;; See ‘(info "(Elisp) Markers")’
;;     https://stackoverflow.com/questions/13564506/navigate-location-history-backwards-step-by-step-in-emacs/13566702#13566702
(defcustom marker-stack-max-depth 30
  "Maximum length of the stacks for navigating the history of
point locations."
  :type 'integer
  :group 'marker-stack)

(defcustom marker-stack-functions-every-call
  '(beginning-of-buffer
    end-of-buffer
    goto-line
    isearch-mode
    mark-whole-buffer
    ;; Remember the position from where query replacement began.
    query-replace
    query-replace-regexp
    ;; By advising ‘replace-match-maybe-edit’ the positions of
    ;; replaced matches are added to the history. Advise
    ;; ‘replace-search’ to remember the positions of both replaced and
    ;; skipped matches.
    replace-match-maybe-edit
    ;; Advise ‘mouse-drag-region’ to save the position on left clicks.
    ;; It is bound to ‘down-mouse-1’ (see
    ;; ‘(describe-key (kbd "<down-mouse-1>"))’), which corresponds to
    ;; pressing the left mouse button.
    ;; NOTE: ‘mouse-set-point’, doesn’t work well here: if it is
    ;; advised, the point locations before and after evaluating
    ;; it are both saved in the backward stack.
    mouse-drag-region)
  "Functions that will be advised by ‘marker-stack-setup-advices’.
The functions in this variable save the point location at every
invocation."
  :type '(repeat function)
  :group 'marker-stack)

(defcustom marker-stack-functions-first-call
  '(backward-paragraph
    forward-paragraph)
  "Functions that will be advised by ‘marker-stack-setup-advices’.
The functions in this variable save the point location only at
the first invocation when called repeatedly."
  :type '(repeat function)
  :group 'marker-stack)

(defvar-local marker-stack-backward nil
  "Stack of previous point locations.")

(defvar-local marker-stack-forward nil
  "Stack with the more recent point locations, populated by
stepping back through the history.")

;;;###autoload
(defun marker-stack-push ()
  "Move the forward stack, reversed, onto the backward stack,
then push the current position onto the backward stack."
  (interactive "^")
  (marker-stack--value 'forward 'move)
  ;; Insert the current position on top of the elements that were in
  ;; the forward stack. This way we can get back to the current
  ;; position before stepping into the other branches of history that
  ;; might depart from it.
  (marker-stack--store 'backward)
  (when (called-interactively-p 'interactive)
    (message "Location saved")))

;;;###autoload
(defun marker-stack-backward-stack-pop ()
  "Push the current position onto the forward stack,
pop the latest position from the backward stack and move to it.
Note that each stack stores only a maximum of
‘marker-stack-max-depth’ positions."
  (interactive "^")
  (if (marker-stack--value 'backward 'dig)
      (progn
        (marker-stack--store 'forward)
        (marker-stack--go 'backward)
        (marker-stack--value 'backward 'cdr))
    (message "No other location in the backward stack")))

;;;###autoload
(defun marker-stack-forward-stack-pop ()
  "Push the current position onto the backward stack,
pop the earliest position from the forward stack and move to it.
Note that each stack stores only a maximum of
‘marker-stack-max-depth’ positions."
  (interactive "^")
  (if (marker-stack--value 'forward 'dig)
      (progn
        (marker-stack--store 'backward)
        (marker-stack--go 'forward)
        (marker-stack--value 'forward 'cdr))
    (message "No other location in the forward stack")))

;; Cf. https://stackoverflow.com/questions/13564506/navigate-location-history-backwards-step-by-step-in-emacs/13566702#13566702
;;     https://www.emacswiki.org/emacs/JumpToPrevPos
(defun marker-stack--store (name)
  (let ((loc (marker-stack--value name 'car)))
    ;; Don’t push the same location twice.
    ;; It is better to compare the positions as numbers rather than
    ;; markers because there is the possibility that the user pushes a
    ;; marker twice from the same position, first from the base buffer
    ;; and then from an indirect buffer. In that case it would be
    ;; necessary to pop the stack (at least) twice to move past that
    ;; position in the indirect buffer.
    (unless (and loc (= loc (point)))
      (marker-stack--value name 'push (point-marker))
      (marker-stack--value name 'cut))))

(defun marker-stack--go (name)
  (let ((loc (marker-stack--value name 'car)))
    (goto-char loc)))

(defun marker-stack--value (name action &optional arg)
  (let* ((key (intern (concat "marker-stack-" (symbol-name name))))
         (stack (symbol-value key)))
    (if (eq action 'car)
        (car stack)
      (set key
           (pcase action
             (`push
              (cons arg stack))
             (`cdr
              (cdr stack))
             (`dig
              ;; Remove the topmost markers that point to the
              ;; point, if any. Return the new stack.
              (while (equal (car stack) (point-marker))
                (setq stack (cdr stack)))
              stack)
             (`move
              ;; Move the forward stack, reversed, onto the
              ;; backward stack. This allows storing branches of
              ;; history within the linear structure of the
              ;; stack. A shortcoming is that
              ;; ‘marker-stack-backward-stack-pop’ skips the
              ;; branching point if it has already encountered
              ;; it.
              ;; When ‘marker-stack--value’ is called with this
              ;; ACTION, NAME is always ‘forward’.
              (dolist (elt stack)
                (marker-stack--value 'backward 'push elt)))
             (`cut
              ;; Drop the bottommost elements if the stack is
              ;; too deep.
              (butlast stack (- (length stack)
                                marker-stack-max-depth))))))))

;;;###autoload
(defun marker-stack-setup-advices ()
  "Advise the functions in ‘marker-stack-functions-every-call’
and ‘marker-stack-functions-first-call’ to call
‘marker-stack-push’ before moving the point. This way the
backward stack can be used as a replacement for the mark ring."
  (mapc (lambda (func)
          (eval
           `(define-advice ,func (:before
                                  (&rest args)
                                  pushing-to-marker-stack)
              "Call ‘marker-stack-push’ before moving the point."
              (marker-stack-push))))
        marker-stack-functions-every-call)
  (mapc (lambda (func)
          (eval
           `(define-advice ,func (:before
                                  (&rest args)
                                  maybe-pushing-to-marker-stack)
              "Call ‘marker-stack-push’ before moving the point.
If the advised function is invoked repeatedly, call
‘marker-stack-push’ only at the first invocation."
              (unless (eq this-command last-command)
                (marker-stack-push)))))
        marker-stack-functions-first-call))

(provide 'marker-stack)

;;; marker-stack.el ends here
