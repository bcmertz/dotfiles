;;; custom-pdf.el --- pdf configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :ensure t
  :defer t
  ;; :bind TODO
  ;; ("<left>" . image-backward-hscroll)
  ;; ("<right>" . image-forward-hscroll)
  ;; ("C-s" . pdf-occur)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  ;; (pdf-tools-install :no-query) TODO
  ;; (require 'pdf-occur)
  )

;;; custom-pdf.el ends here
