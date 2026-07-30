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

(provide 'custom-golang)
;;; custom-golang.el ends here
