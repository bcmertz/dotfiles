;;; custom-css.el --- css config  -*- lexical-binding: t -*-
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
  :custom
  (css-indent-offset 2))

(provide 'custom-css)
;;; custom-css.el ends here
