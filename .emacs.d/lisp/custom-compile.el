;;; custom-compile.el --- compile config
;;;
;;; Commentary:
;;;
;;; build config
;;;
;;; Code:

(defun compile-comint ()
  "Compile but force interactive comint mode."
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'compile)
  )

(use-package compile
  :config
  (setq compilation-scroll-output 1)
  :bind
  ("C-c c" . compile-comint)
  )

;;; custom-compile.el ends here
