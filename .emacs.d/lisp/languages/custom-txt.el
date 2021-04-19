;;; custom-txt.el --- plain text configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package text-mode
  :mode "\\.txt\\'"
  :defer t
  :config
  (add-hook 'text-mode-hook
      (lambda ()
        (setq word-wrap t))
      )
  )

;;; custom-txt.el ends here
