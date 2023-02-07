;;; custom-terminal.el --- terminal config
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package vterm
  :defer t
  :commands vterm--internal
  :bind* (("C-<return>" . vterm-toggle)
          :map vterm-mode-map (("C-c C-t" . vterm-copy-mode))
          ))

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
      (progn
        (display-buffer (vterm-other-window))
        (select-window (get-buffer-window "*vterm*"))))
    )
  )


;; ;; hijack creation of vterm buffer so we can control where it goes
;; ;; and force it to be the outermost buffer on right or bottom of
;; ;; the frame based of the dimensions of that frame
;; (defun vterm-toggle ()
;;   "Toggle vterm."
;;   (interactive)
;;   (defvar vterm-direction nil)
;;   (if (>= (frame-pixel-width) (frame-pixel-height))
;;       (setq vterm-direction 'right)
;;     (setq vterm-direction 'below))
;;   (let ((term-window (get-buffer-window "*vterm*")))
;;     (if term-window
;;         (if (length> (window-prev-buffers term-window) 1)
;;             (switch-to-prev-buffer term-window)
;;           (delete-window term-window))
;;       (vterm--internal #'(lambda (_)))
;;       (if (eq vterm-direction 'right)
;;           (display-buffer (get-buffer "*vterm*") '((display-buffer-in-side-window)
;;                                                    (side . right)
;;                                                    (window-width . 0.38)))
;;         (display-buffer-in-side-window (get-buffer "*vterm*")
;;                                        '((window-height . 0.38)
;;                                          (side . bottom))))
;;       (other-window 1)
;;       )))


;; ;; this approach treats vterm like any other regular buffer and saves
;; ;; the window configuration prior to creation so that we can restore it
;; ;; when we kill it
;; ;; has the downside that if the window configuration changes after creation
;; ;; of a vterm buffer its destruction could take us arbitrarily back in time
;; ;;
;; ;; variable to save window configuration
;; ;;
;; ;; UPDATE: Turns out this is overkill and quit-window is really smart
;; ;; and can do basically all of the heavy lifting required
;; (defvar vterm-window-configuration nil)
;; (defun vterm-toggle ()
;;   "Toggle vterm."
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

(provide 'custom-terminal)
;;; custom-terminal.el ends here
