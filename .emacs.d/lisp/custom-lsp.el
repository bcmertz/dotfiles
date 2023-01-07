;;; custom-lsp.el --- golang configuration
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(use-package eglot
  :defer t
  :init
  (defvar lsp-tool "eglot")
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  )

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'shell-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'eglot-ensure)

;; (use-package lsp-mode
;;   :defer t
;;   :init
;;   (defvar lsp-tool "lsp")
;;   ;; (setq lsp-keymap-prefix "C-c l")
;;   :hook ((go-mode . lsp)
;;          (js-mode . lsp)
;;          (typescript-mode . lsp)
;;          ;; (lsp-mode . flycheck-mode)
;;          ;; (lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (lsp-enable-which-key-integration)
;;   )


;; (use-package lsp-ui
;;   :defer t
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   )

;; (use-package company-lsp
;;   :commands company-lsp
;;   )
;; (push 'company-lsp company-backends)


;;; custom-lsp.el ends here
