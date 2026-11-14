;;; custom-html.el --- html configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; html web-mode taken from zamanski
;;;
;;; Code:

(use-package web-mode
  :defer t
  :mode "\\.html\\'"
  :bind-keymap (("M-;" . nil)
                ("C-c C-l" . nil))
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
