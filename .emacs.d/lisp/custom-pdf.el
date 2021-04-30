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
  :bind
  ((:map pdf-view-mode-map
         ( "C-s" . pdf-occur)
         ( "D" . pdf-annot-delete)
         ( "A" . pdf-annot-list-annotations)
         ( "t" . pdf-annot-add-text-annotation)
         ( "h" . pdf-annot-add-highlight-markup-annotation)

         ))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (doc-view-mode)
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  ;; keyboard shortcuts
  )


;;; custom-pdf.el ends here
