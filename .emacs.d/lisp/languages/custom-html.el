;;; custom-html.el --- html configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; html web-mode taken from zamanski
;;;
;;; Code:

(defun web-comment-line (&optional N)
  "Comment N lines if region is active else `web-mode' comment."
  (interactive "p")
  (if (region-active-p)
      (comment-line N)
    (web-mode-comment-or-uncomment)))

(use-package web-mode
  :defer t
  :mode "\\.html\\'"
  :bind (:map web-mode-map
              ("M-;" . web-comment-line)
              ("C-c C-l" . org-store-link))
  :config
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t))

(provide 'custom-html)
;;; custom-html.el ends here
