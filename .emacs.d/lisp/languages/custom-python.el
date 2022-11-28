;;; custom-python.el --- python config
;;;
;;; Commentary:
;;;
;;; python config
;;;
;;; Code:

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "<backtab>") #'company-complete))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;; custom-python.el ends here
