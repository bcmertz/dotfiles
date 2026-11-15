;;; custom-latex.el --- latex configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; latex config
;;;
;;; Code:

(defun compile-latex-document ()
  "Compile latex document."
  (TeX-command-run-all nil))

(defun setup-latex-compile-on-save-hook ()
  "Setup compile on save hook for latex."
  (interactive)
  (add-hook 'after-save-hook 'compile-latex-document nil 'local))

;; M-x prettify-symbols-mode to display pretty math
;; C-c C-a to compile and view
(use-package tex
  :defer t
  :straight auctex
  :mode "\\*.tex\\'"
  :hook ((LaTex-Mode . setup-latex-compile-on-save-hook)
         (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))
  :bind (:map TeX-mode-map
              ("C-C C-a" . TeX-command-run-all))
  :config
  ;; use pdfview with tex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t))

(provide 'custom-latex)
;;; custom-latex.el ends here
