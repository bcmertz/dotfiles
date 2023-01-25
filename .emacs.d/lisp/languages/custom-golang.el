;;; custom-golang.el --- golang configuration
;;;
;;; Commentary:
;;;
;;; snippets, formatting, docs, autocompletion, etc
;;;
;;; Code:
;;;
;;; TODO - eglot save hook add/remove imports, currently broken


(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
  (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)
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

;; https://gist.github.com/carlosrogue/777f43b4a46400cae21aaf9ba5ca5ccc
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

;; eglot-organize-imports is hopefully a temporary stopgap until
;; https://github.com/joaotavora/eglot/issues/574 is addressed.
(defun eglot-organize-imports ()
  "Offer to execute the source.organizeImports code action."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions (jsonrpc-request
                   server
                   :textDocument/codeAction
                   (list :textDocument (eglot--TextDocumentIdentifier))))
         (action (cl-find-if
                  (jsonrpc-lambda (&key kind &allow-other-keys)
                    (string-equal kind "source.organizeImports" ))
                  actions)))
    (when action
      (eglot--dcase action
        (((Command) command arguments)
          (eglot-execute-command server (intern command) arguments))
        (((CodeAction) edit command)
          (when edit (eglot--apply-workspace-edit edit))
          (when command
            (eglot--dbind ((Command) command arguments) command
              (eglot-execute-command server (intern command) arguments))))))))

(defun eglot-organize-imports-on-save ()
  (defun eglot-organize-imports-nosignal ()
    "Run eglot-organize-imports, but demote errors to messages."
    ;; Demote errors to work around
    ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
    ;; so that we do not prevent subsequent save hooks from running
    ;; if we encounter a spurious error.
    (with-demoted-errors "Error: %s" (eglot-organize-imports)))
  (add-hook 'before-save-hook #'eglot-organize-imports-on-save))

;; DEPRECATED? - old way of fixing imports and formatting on save
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-save-hook ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; (defun eglot-interactively-organize-imports ()
;;   (call-interactively 'eglot-code-action-organize-imports))

;; (defun eglot-go-save-hook ()
;;   (add-hook 'before-save-hook #'eglot-format-buffer -30 t)
;;   (add-hook 'before-save-hook #'eglot-interactively-organize-imports nil t)
;;   )

(provide 'custom-golang)
;;; custom-golang.el ends here
