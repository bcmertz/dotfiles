;;; custom-lsp.el --- golang configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; LSPs provide IDE like functionality - renaming variables, completion, error checking
;;; which are digested through builtin functions in our LSP client (eg - eglot-rename)
;;; or are digested by other libraries like flymake (error-checking) or corfu (completion)
;;;
;;; Code:

(use-package eglot
  :defer t
  :init
  (defvar lsp-tool "eglot")
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  )

;; turn off JSONRPC debug event log mechanism.
;; helps speed up eglot
;; https://www.reddit.com/r/emacs/comments/1447fy2/looking_for_help_in_improving_typescript_eglot/
(fset #'jsonrpc--log-event #'ignore)

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

(provide 'custom-lsp)
;;; custom-lsp.el ends here
