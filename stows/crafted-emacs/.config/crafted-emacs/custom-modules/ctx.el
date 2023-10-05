;;; ctx.el --- Witek's Context                       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Witoslaw Koczewski

;; Author: Witoslaw Koczewski <wi@koczewski.de>

;; (defcustom ctx-ring-max 16
;; "Maximum size of point ring.  \
;; Start discarding off end if gets this big."
;; :type 'integer
;; :group 'ctx)

(defvar ctx-ring nil
  "The list of saved points, most recent first.")

(defvar ctx-point nil)

(defun ctx-push ()
  "Push point to CTX-RING."
  (interactive)

  ;; (add-to-history
  ;; 'ctx-ring (copy-marker (point-marker)) ctx-ring-max t)
  ;; (add-to-list 'ctx-ring (copy-marker (point-marker)))
  (setq ctx-ring (nconc ctx-ring (list (copy-marker (point-marker)))))

  (setq ctx-point (copy-marker (point-marker)))

  (message "Point saved")
  )

(defun ctx-jump ()
  (interactive)

  (let ((options (mapcar (lambda (marker)
                           (cons (format "string %s" marker)
                                 marker))
                         ctx-ring))
        (completion-extra-properties '(:annotation-function ctx--mark-annotation-function)))
    (let* (
           (selected (completing-read "Jump to: " options nil t))
           (marker (cdr (assoc selected options))))
      (message (format "Jump to %s" marker))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)

      )
  )
  )


(defun ctx--mark-annotation-function (candidate)
  (format "%s" (cdr (assoc candidate options)))
  )

(defun ctx-pop ()
  (interactive)

  ;; (set-marker (mark-marker) ctx-point)

  ;; (switch-to-buffer (marker-buffer ctx-point))
  ;; (goto-char ctx-point)

  ;; (setq ctx-ring (nconc ctx-ring (list (copy-marker (mark-marker)))))
  (goto-char (car ctx-ring))
  ;; (set-marker (mark-marker) (car ctx-ring))
  ;; (set-marker (car ctx-ring) nil)
  ;; (pop ctx-ring)

  ;; (message ctx-ring)
  )

(provide 'ctx)
