;;; custom-latex.el --- latex configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; M-x prettify-symbols-mode to display pretty math
;; C-c C-a to compile and view
(use-package tex
  :defer t
  :straight auctex)

;; to use pdfview with tex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(provide 'custom-latex)
;;; custom-latex.el ends here
