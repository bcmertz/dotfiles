;;;;;;;;;;;;;;;;;;;;;Flycheck Configuration;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-check-syntax-automatically '(save))
  ;; disable documentation related emacs lisp checker
  (setq-default flycheck-disabled-checkers '())
  (setq flycheck-mode-line-prefix "✔"))

(provide 'custom-flycheck)
