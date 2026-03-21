;;; custom-error-check.el --- syntax checking -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; syntax checking
;;;
;;; Code:

(use-package flymake
  :defer t
  :init
  (add-hook 'prog-mode-hook #'flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)
              ("C-c e s" . flymake-start)
              ("C-c e i" . flymake-show-diagnostic)
              ("C-c e l" . flymake-show-buffer-diagnostics)
              ("C-c e m" . flymake-mode))
  :config
  (which-key-add-key-based-replacements "C-c e" "errors"))

(provide 'custom-error-check)
;;; custom-error-check.el ends here
