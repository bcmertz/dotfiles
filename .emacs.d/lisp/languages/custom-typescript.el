;;; custom-typescript.el --- js configuration
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(use-package typescript-mode
  :defer t
  :init
  (setq typescript-indent-level 2)
  )

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;;; custom-typescript.el ends here
