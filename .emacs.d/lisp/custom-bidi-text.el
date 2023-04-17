;;; custom-bidi-text.el --- configuration handling rtl & ltr text -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; sourced from
;;; https://www.youtube.com/watch?v=LxuNmeCNnqU
;;; &&
;;; https://www.youtube.com/watch?v=y3oLG-6KTaE
;;;
;;; Code:

;; Bidirectional automatic reordering,
;; keep it on to start with, toggle off as needed
(setq-default bidi-display-reordering t)

(defun bidi-reordering-toggle-dont-mess-with ()
  "Toggle bidirectional display reordering."
  (interactive)
  (setq bidi-display-reordering (not bidi-display-reordering))
  (message "bidi reordering is %s" bidi-display-reordering)
  )

;; default display reordering to on for the following hooks
;; (defun bidi-display-reordering-on ()
;;   "Set bidi-display-reportering on."
;;   (setq-local bidi-display-reordering t))
;; (add-hook 'text-mode-hook 'bidi-display-reordering-on)
;; (add-hook 'org-mode-hook 'bidi-display-reordering-on)

;; Paragraph direction
;; By default emacs tries to guess the Paragraph directorion for bidi-text.
;; This ends up with mixed up looking buffers.
(setq-default bidi-paragraph-direction 'left-to-right)

(defun bidi-ltr-toggle ()
  "Will switch the explicit direction of text for the current buffer.  This will set 'bidi-display-reordering' to t."
  (interactive "")
  (setq-default bidi-display-reordering t)
  (if (equal bidi-paragraph-direction 'right-to-left)
      (setq bidi-paragraph-direction 'left-to-right)
    (setq bidi-paragraph-direction 'right-to-left)
    )
  (message "%s" bidi-paragraph-direction))

(provide 'custom-bidi-text)
;;; custom-bidi-text.el ends here
