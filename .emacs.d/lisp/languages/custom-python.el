;;; custom-python.el --- python config
;;;
;;; Commentary:
;;;
;;; python config
;;;
;;; Code:

(use-package python
  :defer t
  :mode "\\*.py\\'"
  :bind ((:map python-mode-map
               ("<backtab>" . completion-at-point))
         (:map python-ts-mode-map
               ("<backtab>" . completion-at-point)))
  )


;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

(provide 'custom-python)
;;; custom-python.el ends here
