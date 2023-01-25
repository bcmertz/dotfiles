;;; custom-python.el --- python config
;;;
;;; Commentary:
;;;
;;; python config
;;;
;;; Code:

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "<backtab>") #'completion-at-point))

;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

(provide 'custom-python)
;;; custom-python.el ends here
