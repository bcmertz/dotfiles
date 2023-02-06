;;; custom-terminal.el --- terminal config
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :bind* (:map vterm-mode-map (("C-c C-t" . vterm-copy-mode))
               ;; ("C-<return>" . vterm-toggle)
               ))

;; (defvar vterm-window-configuration nil)
;; (defun vterm-toggle ()
;;   "Toggle vterm"
;;   (interactive)
;;   (if (get-buffer "*vterm*")
;;       ;; if vterm exists
;;       (if (get-buffer-window "*vterm*")
;;           ;; and is visible
;;           (progn
;;             ;; revert window configuration back to prior to vterm
;;             (set-window-configuration vterm-window-configuration)
;;             (setq vterm-window-configuration nil))
;;         ;; and isn't visible
;;         (progn
;;           ;; save window configuration and display vterm
;;           (setq vterm-window-configuration (current-window-configuration))
;;           (display-buffer "*vterm*")))
;;     ;; if vterm doesn't exist
;;     (progn
;;       ;; save window configuration and start vterm
;;       (setq vterm-window-configuration (current-window-configuration))
;;       (display-buffer (vterm-other-window)))
;;     )
;;   )

(add-to-list 'display-buffer-alist
     '("\*vterm\*"
       (display-buffer-in-side-window)
       (window-width . 0.45)
       (side . right)
       (slot . 0)))

(provide 'custom-terminal)
;;; custom-terminal.el ends here
