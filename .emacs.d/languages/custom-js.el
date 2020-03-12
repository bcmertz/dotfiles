;;;;;;;;;;;;;;;;;;;;;JS Configuration;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js
  :defer t
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-mode-map (kbd "C-c C-f") 'js-find-symbol)
  (require 'yasnippet)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (yas-global-mode 1))

(setq js-indent-level 2)

(use-package flycheck
  :ensure t
  :mode "\\.js\\'"
  :init
  (progn
    (setq flycheck-highlighting-mode 'lines)
    (defun setup-js-mode ()
      (flycheck-select-checker 'javascript-eslint)
      (flycheck-mode))

    (add-hook 'js-mode-hook #'setup-js-mode)))
