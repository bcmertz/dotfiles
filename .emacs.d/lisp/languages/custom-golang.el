;;; custom-golang.el --- golang configuration
;;;
;;; Commentary:
;;;
;;; snippets, formatting, docs, autocompletion, etc
;;;
;;; Code:

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :hook (go-mode . go-mode-save-hook)
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

(defun go-mode-save-hook ()
  "Internal function to format buffer organize imports."
  (add-hook 'before-save-hook #'eglot-format-buffer t t)
  (add-hook 'before-save-hook #'(lambda () (eglot-code-action-organize-imports 1) ) t t))

(provide 'custom-golang)
;;; custom-golang.el ends here
