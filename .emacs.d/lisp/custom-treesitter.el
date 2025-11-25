;;; custom-treesitter.el --- tree-sitter configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; tree-sitter uses defined language grammars to have understanding of the syntax
;;; of a program, allowing for nice syntax highlighting and structural editing
;;;
;;; Code:

(use-package treesit
  :defer t
  :straight (:type built-in))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ts code folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package ts-fold
;;   :straight (ts-fold :type git
;;                      :host github
;;                      :repo "emacs-tree-sitter/ts-fold")
;;   :init
;;   (global-ts-fold-mode)
;;   (global-ts-fold-indicators-mode)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ts code movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package ts-movement
;;   :straight (ts-movement :type git
;;                          :host github
;;                          :repo "haritkapadia/ts-movement")
;;   :hook
;;   (bash-ts-mode-hook . ts-movement-mode)
;;   (c++-ts-mode-hook . ts-movement-mode)
;;   (c-ts-mode-hook . ts-movement-mode)
;;   (cmake-ts-mode-hook . ts-movement-mode)
;;   (csharp-ts-mode-hook . ts-movement-mode)
;;   (css-ts-mode-hook . ts-movement-mode)
;;   (dockerfile-ts-mode-hook . ts-movement-mode)
;;   (go-mod-ts-mode-hook . ts-movement-mode)
;;   (go-ts-mode-hook . ts-movement-mode)
;;   (java-ts-mode-hook . ts-movement-mode)
;;   (js-ts-mode-hook . ts-movement-mode)
;;   (json-ts-mode-hook . ts-movement-mode)
;;   (python-ts-mode-hook . ts-movement-mode)
;;   (ruby-ts-mode-hook . ts-movement-mode)
;;   (rust-ts-mode-hook . ts-movement-mode)
;;   (toml-ts-mode-hook . ts-movement-mode)
;;   (tsx-ts-mode-hook . ts-movement-mode)
;;   (typescript-ts-mode-hook . ts-movement-mode)
;;   (yaml-ts-mode-hook . ts-movement-mode)
;;   )

;; (use-package combobulate
;;   ;; Ensure `combobulate-mode` is activated when you launch a mode it supports
;;   :hook ((python-mode . combobulate-mode)
;;          (js-mode . combobulate-mode)
;;          (typescript-mode . combobulate-mode))
;;   :straight (combobulate :type git
;;                          :host github
;;                          :repo "mickeynp/combobulate")
;;   )

(provide 'custom-treesitter)
;;; custom-treesitter.el ends here
