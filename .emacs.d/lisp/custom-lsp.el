;;; custom-lsp.el --- golang configuration
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(use-package lsp-mode
  :defer t
  :ensure t
  :init
  (defvar lsp-tool "lsp")
  ;; (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         ;; (lsp-mode . flycheck-mode)
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (lsp-enable-which-key-integration)
  )


(use-package lsp-ui
  :defer t
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  )

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   )
;; (push 'company-lsp company-backends)

;; (use-package eglot
;;   :defer t
;;   :ensure t
;;   :init
;;   (defvar lsp-tool "eglot")
;;   )

;; ;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))

;; ;; gopls
;; (add-hook 'go-mode-hook 'eglot-ensure)

;; ;; javascript-typescript-langserver
;; (add-hook 'js-mode-hook 'eglot-ensure)
;; (add-hook 'typescript-mode-hook 'eglot-ensure)

;; ;; remove flymake dependency that hides flycheck info in modeline
;; (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

;;; custom-lsp.el ends here
