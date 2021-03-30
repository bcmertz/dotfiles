;;; custom-arduino.el --- arduino configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package arduino-mode
  :mode "\\.ino\\'"
  :defer t
  :ensure t)

;; make M-x work in serial terminal
(eval-after-load 'term
  '(define-key term-raw-map (kbd "M-x") #'execute-extended-command))

;;; custom-arduino.el ends here
