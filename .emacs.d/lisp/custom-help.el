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


;;; custom-help.el ends here
