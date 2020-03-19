;;; custom-flycheck.el --- syntax checking
;;;
;;; Commentary:
;;;
;;; syntax checking
;;;
;;; Code:

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-check-syntax-automatically '(save))
  ;; disable documentation related emacs lisp checker
  (setq-default flycheck-disabled-checkers '(go-staticcheck))
  (setq flycheck-mode-line-prefix "✔"))

;;; custom-flycheck.el ends here
