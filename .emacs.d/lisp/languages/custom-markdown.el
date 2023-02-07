;;; custom-markdown.el --- markdown configuratiob
;;;
;;; Commentary:
;;;
;;; markdown preview
;;;
;;; Code:
(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("C-c m" . markdown-live-preview-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :general
  (tyrant-def markdown-mode-map
    "mp"  'markdown-live-preview-mode)
  )

(provide 'custom-markdown)
;;; custom-markdown.el ends here
