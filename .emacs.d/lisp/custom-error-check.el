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
  :config
  (which-key-add-key-based-replacements "C-c !" "flymake")
  (global-set-key (kbd "C-c ! n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") 'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! s") 'flymake-start)
  (global-set-key (kbd "C-c ! i") 'flymake-show-diagnostic)
  (global-set-key (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! m") 'flymake-mode)
  )

(provide 'custom-error-check)
;;; custom-error-check.el ends here
