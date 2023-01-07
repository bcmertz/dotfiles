;;; custom-js.el --- js configuration
;;;
;;; Commentary:
;;;
;;; snippets, keybindings
;;;
;;; Code:
(use-package js
  :defer t
  :mode "\\*.js\\'"
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-mode-map (kbd "C-c C-f") 'js-find-symbol)
  :general
  (tyrant-def js-mode-map
    "mf"  'js-find-symbol)
  )

;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode))
(setq js-indent-level 2)

;;; custom-js.el ends here
