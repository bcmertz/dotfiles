;;; custom-css.el --- css config
;;;
;;; Commentary:
;;;
;;; css config
;;;
;;; Code:
(use-package css
  :straight (:type built-in)
  :defer t
  :mode "\\*.css\\'"
  )

(setq css-indent-offset 2)

(provide 'custom-css)
;;; custom-css.el ends here
