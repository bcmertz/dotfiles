;;; custom-compile.el --- compile config
;;;
;;; Commentary:
;;;
;;; build config
;;;
;;; Code:

(defun compile-comint ()
  "Compile but force interactive comint mode.
This allows us to enter sudo password.
https://stackoverflow.com/a/3612017"
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
