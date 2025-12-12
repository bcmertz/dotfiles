;;; custom-compile.el --- compile config -*- lexical-binding: t -*-
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
  :bind (:map prog-mode-map
              ("C-c c" . compile-comint)
              ))

(provide 'custom-compile)
;;; custom-compile.el ends here
