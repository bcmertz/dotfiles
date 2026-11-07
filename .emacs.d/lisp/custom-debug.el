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
    (setq debug-on-error t)))

;; which key prefix for debug related keybindings
(which-key-add-key-based-replacements "C-c d" "debug")

(global-set-key (kbd "C-c d r") 'toggle-debug-on-error)

(provide 'custom-debug)
;;; custom-debug.el ends here
