;;;;;;;;;;;;;;;;;;;;;JS Configuration;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :defer t
  :mode "\\*.js\\'"
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-mode-map (kbd "C-c C-f") 'js-find-symbol)
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (yas-global-mode 1)
  :general
  (tyrant-def js-mode-map
    "mf"  'js-find-symbol)
  )


(setq js-indent-level 2)

(provide 'custom-js)
