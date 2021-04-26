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
  ;; TODO l/r pdf scrolling
  ;; ("<left>" . image-backward-hscroll)
  ;; ("<right>" . image-forward-hscroll)
  ("C-s" . pdf-occur)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (doc-view-mode)
  (pdf-tools-install :no-query)
  (require 'pdf-occur)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "C-s") 'pdf-occur)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "A") 'pdf-annot-list-annotations)
  )


;;; custom-pdf.el ends here
