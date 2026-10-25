;;; custom-debug.el --- debug configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; debug system problems
;;;
;;; Code:

;; toggle debug on error
(defun toggle-debug-on-error ()
  "Toggle debug on error."
  (interactive)
  (if (eq debug-on-error t)
      (setq debug-on-error nil)
    (setq debug-on-error t))
  )

;; ;; toggle debug on event
;; (defun toggle-debug-on-event ()
;;   "Toggle debug on event."
;;   (interactive)
;;   (if (eq debug-on-event t)
;;       (setq debug-on-event nil)
;;     (setq debug-on-event t))
;;   )

;; which key prefix for debug related keybindings
(which-key-add-key-based-replacements "C-c d" "debug")

(global-set-key (kbd "C-c d r") 'toggle-debug-on-error)
;; (global-set-key (kbd "C-c d v") 'toggle-debug-on-event)



(provide 'custom-debug)
;;; custom-debug.el ends here
