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
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (yas-global-mode 1)
  (company-mode)
  :general
  (tyrant-def js-mode-map
    "mf"  'js-find-symbol)
  )

;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode))
(setq js-indent-level 2)

;;; custom-js.el ends here
