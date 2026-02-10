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
  :config
  ;; (setq python-flymake-command '("flake8" "-"))
  (setq python-flymake-command '("pyflakes3"))
  )

(provide 'custom-python)
;;; custom-python.el ends here
