;;; custom-special.el --- customize special mode -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Customize special mode and derived modes
;;;
;;; Code:

(use-package special-mode
  :straight (:type built-in)
  :bind (:map special-mode-map
              ("Q" . quit-window)
              ("q" . kill-buffer-and-window)))

(provide 'custom-special)
;;; custom-special.el ends here
