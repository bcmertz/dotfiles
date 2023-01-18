;;; custom-treesitter.el --- tree-sitter configuration
;;;
;;; Commentary:
;;;
;;; tree-sitter
;;;
;;; Code:

(use-package tree-sitter
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :config
  ;; always turn on tree-sitter-hl-mode
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; prevent bolding function calls when the cursor isn't on them
  (set-face-attribute 'tree-sitter-hl-face:function.call nil :weight 'normal))

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


;;; custom-treesitter.el ends here
