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
              ;; C-e goes to the end of the visual line not the logical line
              (turn-on-visual-line-mode)
              ;; check spelling on the fly
              (flyspell-mode 1)
              ;; dont have super long lines, break them
              (setq word-wrap t)
              )
            )
  )

(global-set-key (kbd "C-i") 'ispell-word)
(global-set-key (kbd "M-i") 'ispell-buffer)


;;; custom-txt.el ends here
