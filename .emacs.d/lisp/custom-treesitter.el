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

;; code folding
;; (use-package ts-fold
;;   :straight (ts-fold :type git
;;                      :host github
;;                      :repo "emacs-tree-sitter/ts-fold")
;;   :init
;;   (global-ts-fold-mode)
;;   (global-ts-fold-indicators-mode)
;;   )

;;; custom-treesitter.el ends here
