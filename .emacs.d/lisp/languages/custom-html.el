;;; custom-html.el --- html configuration
;;;
;;; Commentary:
;;;
;;; html web-mode taken from zamanski
;;;
;;; Code:

(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)) ; this fixes the quote problem I mentioned

(add-hook 'web-mode-hook
      (lambda ()
        (local-unset-key (kbd "C-c C-l"))))

(provide 'custom-html)
;;; custom-html.el ends here
