;;;;;;;;;;;;;;;;;;;;;JS Configuration;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :defer t
  :config
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (yas-global-mode 1))

(setq js-indent-level 2)


