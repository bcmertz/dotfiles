;;; custom-terminal.el --- terminal config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :commands vterm--internal
  :bind (("C-<return>" . vterm-toggle)
         :map vterm-mode-map (("C-c C-t" . vterm-copy-mode))))

(defun vterm-toggle ()
  "Toggle vterm."
  (interactive)
  (let ((term-buffer (get-buffer "*vterm*"))
        (term-window (get-buffer-window "*vterm*")))
    (if term-buffer
        ;; if vterm exists
        (if term-window
            ;; and is visible
            (quit-window nil term-window)
          ;; and isn't visible
          (progn
            (display-buffer term-buffer)
            (select-window (get-buffer-window "*vterm*"))))
      ;; if vterm doesn't exist
      (vterm--internal #'display-buffer))))

(provide 'custom-terminal)
;;; custom-terminal.el ends here
