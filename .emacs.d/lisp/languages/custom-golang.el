;;; custom-golang.el --- golang configuration
;;;
;;; Commentary:
;;;
;;; snippets, formatting, docs, autocompletion, etc
;;;
;;; Code:

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-save-hook ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(defun eglot-interactively-organize-imports ()
  (call-interactively 'eglot-code-action-organize-imports))

(defun eglot-go-save-hook ()
  (add-hook 'before-save-hook #'eglot-format-buffer -30 t)
  (add-hook 'before-save-hook #'eglot-interactively-organize-imports -20 t)
  )


(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :init
  (progn
    (if (equal lsp-tool "lsp")
        (add-hook 'go-mode-hook #'lsp-go-save-hook)
      (if (equal lsp-tool "eglot")
          (add-hook 'go-mode-hook #'eglot-go-save-hook)
        )
      )
    )

  :config
  (local-set-key (kbd "C-c C-r") 'go-rename)
  (local-set-key (kbd "C-c C-p") 'godoc-at-point)
  (local-set-key (kbd "C-c C-d") 'godef-describe)
  (local-set-key (kbd "C-c C-t") 'gocode-toggle)
  :general
  (tyrant-def go-mode-map
    "mr"  'go-rename
    "mp"  'godoc-at-point
    "md" 'godef-describe
    "mt"  'gocode-toggle))

;;; custom-golang.el ends here
