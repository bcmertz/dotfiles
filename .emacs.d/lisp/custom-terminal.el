;;; custom-terminal.el --- terminal config
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :bind* (("C-<return>" . toggle-vterm)
          :map vterm-mode-map (("C-c C-t" . vterm-copy-mode))))

(defun toggle-vterm ()
  "Toggle vterm"
  (interactive)
  (if (string= (buffer-name) "*vterm*")
      (switch-to-prev-buffer)
    (vterm)))

(provide 'custom-terminal)
;;; custom-terminal.el ends here
