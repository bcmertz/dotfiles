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
  :straight auctex
  :mode "\\*.tex\\'"
  )

;; to use pdfview with tex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(defun compile-latex-document ()
  "Compile latex document."
  (TeX-command-run-all nil)
  )

;; look hook to recompile after saving
(add-hook 'LaTeX-mode-hook
          (lambda () (add-hook 'after-save-hook 'compile-latex-document nil 'local)))

(provide 'custom-latex)
;;; custom-latex.el ends here
