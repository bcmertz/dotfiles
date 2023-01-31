;;; custom-terminal.el --- terminal config
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :bind* (("C-<return>" . vterm)
          :map vterm-mode-map (("C-c C-t" . vterm-copy-mode))))

(provide 'custom-terminal)
;;; custom-terminal.el ends here
