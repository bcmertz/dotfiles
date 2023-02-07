;;; custom-help.el --- customize help mode
;;;
;;; Commentary:
;;;
;;; Customize help mode
;;;
;;; Code:

(use-package help
  :straight nil
  :bind (:map help-mode-map
         ("C-<left>" . help-go-back)
         ("C-<right>" . help-go-forward)
         ("i" . help-goto-info)
         ("n" . help-goto-next-page)
         ("I" . help-goto-lispref-info)
         ("p" . help-goto-previous-page)
         ))

;; this example would cause the help buffer to always
;; be the bottom most buffer, full-width, with a height
;; 25% of the frame
;;
;; (add-to-list 'display-buffer-alist
;;      '("\*Help\*"
;;        (display-buffer-in-side-window)
;;        (window-height . (/ (frame-height) 0.5))
;;        (side . bottom)
;;        (slot . 0)))

(provide 'custom-help)
;;; custom-help.el ends here
