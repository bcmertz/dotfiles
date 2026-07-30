;;; custom-help.el --- customize help mode -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Customize help mode
;;;
;;; Code:

(use-package help
  :straight (:type built-in)
  :bind (:map help-mode-map
         ("C-<left>" . help-go-back)
         ("C-<right>" . help-go-forward)
         ("i" . help-goto-info)
         ("n" . help-goto-next-page)
         ("q" . kill-buffer-and-window)
         ;; ("q" . kill-current-buffer)
         ("I" . help-goto-lispref-info)
         ("p" . help-goto-previous-page)
         ))

(defun quit-helpful ()
  "Quit helpful buffer and kill other helpful buffers."
  (interactive)
  (helpful-kill-buffers)
  (delete-window)
  )

(use-package helpful
  :defer t
  :straight (helpful :type git
                     :host github
                     :repo "daanturo/helpful"
                     :branch "go-backward")
  :bind* (("C-h f" . #'helpful-function)
          ("C-h F" . #'helpful-callable)
          ("C-h v" . #'helpful-variable)
          ("C-h k" . #'helpful-key)
          ("C-h o" . #'helpful-symbol)
          ("C-h x" . #'helpful-command)
          (:map helpful-mode-map
                ("q" . quit-helpful)
                ;; ("q" . kill-current-buffer)
                ("C-<left>" . helpful-go-back)
                ("C-<right>" . helpful-go-forward))))

(provide 'custom-help)
;;; custom-help.el ends here
