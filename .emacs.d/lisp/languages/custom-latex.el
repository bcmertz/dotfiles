;;; custom-latex.el --- latex configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; C-c C-a to compile and view directly
(use-package tex
  :defer t
  :ensure auctex)

;; to use pdfview with tex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;; custom-latex.el ends here
