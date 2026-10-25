;;; custom-text.el --- basic non-programming mode configuration
;;;
;;; Commentary:
;;;
;;; Handle text and special basic modes
;;;
;;; Code:

;; toggle truncate lines
(defun toggle-truncate-lines ()
  "Toggle `truncate-lines'."
  (interactive)
  (if (eq truncate-lines t)
      (setq-local truncate-lines nil)
    (setq-local truncate-lines t))
  )
(global-set-key (kbd "C-c t w") 'toggle-truncate-lines)

(use-package text-mode
  :straight (:type built-in)
  :config
  ;; wrap lines
  (setq truncate-lines nil)
  ;; C-e goes to the end of the visual line not the logical line
  (turn-on-visual-line-mode)
  ;; no hl-line-mode locally
  (setq-local global-hl-line-mode nil)
  ;; line cursor instead of box
  (setq-local cursor-type 'bar)
  ;; dont have super long lines, break them
  (setq word-wrap t))


(provide 'custom-text)
;;; custom-text.el ends here
