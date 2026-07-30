;;; custom-text.el --- basic non-programming mode configuration
;;;
;;; Commentary:
;;;
;;; Handle text and special basic modes
;;;
;;; Code:

(defun setup-text-mode ()
  "Setup text mode."
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

(use-package text-mode
  :straight (:type built-in)
  :hook (text-mode . setup-text-mode))

;; (use-package special-mode
;;   :straight (:type built-in)
;;   :hook (special-mode . setup-text-mode))

(provide 'custom-text)
;;; custom-text.el ends here
