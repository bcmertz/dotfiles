;;; custom-compile.el --- compile config
;;;
;;; Commentary:
;;;
;;; build config
;;;
;;; Code:

(use-package compile
  :config
  (setq compilation-scroll-output 1)
  :bind
  ("C-c c" . compile)
  )

;;; custom-compile.el ends here
