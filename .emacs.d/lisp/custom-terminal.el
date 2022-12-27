;;; custom-terminal.el --- terminal config
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :bind*
  (("C-<return>" . vterm)
   ("C-S-<return>" . vterm-other-window)))


;;; custom-terminal.el ends here
