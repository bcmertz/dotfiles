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
  ;; C-e goes to the end of the visual line not the logical line
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  ;; dont have super long lines, break them
  (add-hook 'text-mode-hook
      (lambda ()
        (setq word-wrap t)
        )
      )
  )

;;; custom-txt.el ends here
