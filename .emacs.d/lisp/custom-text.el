;;; custom-text.el --- basic non-programming mode configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Handle text and special basic modes
;;;
;;; Code:

(defun turn-on-text-mode ()
  ;; wrap lines
  (setq truncate-lines nil)
  ;; C-e goes to the end of the visual line not the logical line
  (turn-on-visual-line-mode)
  (message "Here")
  ;; no hl-line-mode locally
  (setq-local global-hl-line-mode nil)
  ;; line cursor instead of box
  (setq-local cursor-type 'bar)
  ;; dont have super long lines, break them
  (setq word-wrap t))

(use-package text-mode
  :straight (:type built-in)
  :hook (text-mode . turn-on-text-mode))

(provide 'custom-text)
;;; custom-text.el ends here
