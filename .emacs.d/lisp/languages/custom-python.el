;;; custom-python.el --- python config
;;;
;;; Commentary:
;;;
;;; python config
;;;
;;; Code:

(use-package python
  :defer t
  :mode "\\*.py*.\\'"
  :bind ((:map python-mode-map
               ("<backtab>" . completion-at-point))
         (:map python-ts-mode-map
               ("<backtab>" . completion-at-point)))
  :config
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; First installed command will be set as `python-flymake-command'
  ;;
  ;; Use ruff check even though it is setup in eglot using `rass python` which sets up ruff and ty
  ;; lsp servers. We do this because otherwise flymake will setup another flymake backend that duplicates
  ;; function, and calling ruff check here basically does nothing.
  (setq python-flymake-commands '(("ruff" "check") ("flake8" "-") ("pyflakes") ("pyflakes3k")))
  (cl-loop for c in python-flymake-commands
           do (if (executable-find (car c))
                  (progn
                    (setq python-flymake-command c)
                    (cl-return)
                    )
                )
           )
  )

(provide 'custom-python)
;;; custom-python.el ends here
