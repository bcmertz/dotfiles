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
  )

(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)

;; turn off JSONRPC debug event log mechanism.
;; helps speed up eglot
;; https://www.reddit.com/r/emacs/comments/1447fy2/looking_for_help_in_improving_typescript_eglot/
;; maybe no longer necessary as of v30?
;; https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots/
;;(fset #'jsonrpc--log-event #'ignore)

(provide 'custom-lsp)
;;; custom-lsp.el ends here
