;;; custom-lsp.el --- golang configuration
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

;;; custom-lsp.el ends here

(use-package lsp-mode
  :hook ((go-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         ;; (lsp-mode . flycheck-mode)
         ;; (lsp-mode . lsp-enable-which-key-integration)
         ))
(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable t)
  )

(use-package company-lsp
  :commands company-lsp
  )
(push 'company-lsp company-backends)
