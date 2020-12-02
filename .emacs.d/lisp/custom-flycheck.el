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
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    ;; (defun setup-js-mode ()
    ;;   (flycheck-select-checker 'javascript-eslint))
    ;; (add-hook 'js-mode-hook #'setup-js-mode)
    )
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  (setq-default flycheck-check-syntax-automatically '(save))
  ;; disable documentation related emacs lisp checker
  (setq-default flycheck-disabled-checkers '(go-staticcheck))
  (setq flycheck-mode-line-prefix "✔"))

;;; custom-flycheck.el ends here
