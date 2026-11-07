;;; custom-elisp.el --- custom elisp -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; fontify elisp more semantically
;; (setq elisp-fontify-semantically t)

;; start elisp script and enable elisp mode
(defun start-elisp-script ()
  "Start elisp script by inserting header and entering elisp mode."
  (interactive)
  (let ((package (car (split-string (buffer-name) "\\.")))
        (type (car (last (split-string (car (split-string (buffer-name) "\\.")) "\\-")))))
    (insert (format ";;; %s --- custom %s -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:


(provide '%s)
;;; %s.el ends here" (buffer-name) type package package))
    ))

(provide 'custom-elisp)
;;; custom-elisp.el ends here
