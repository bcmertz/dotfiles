;;; custom-modeline.el --- custom modeline -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; custom modeline config
;;;
;;; Code:

(use-package doom-modeline
  :config
  (setq doom-modeline-check 'simple)
  (setq doom-modeline-vcs-max-length 100)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-buffer-encoding nil)
  (line-number-mode)
  (column-number-mode)
  :init
  (defface doom-modeline-bar-inactive
    '((t (:background unspecified)))
    "Face used for the inactive bar."
    :group 'doom-modeline-faces)
  (doom-modeline-mode 1))

(use-package hide-mode-line
  :defer t
  :bind (("C-c t M" . global-hide-mode-line-mode)))

(provide 'custom-modeline)
;;; custom-modeline.el ends here
