;;; custom-org.el --- org mode config
;;;
;;; Commentary:
;;;
;;; org mode
;;;
;;; Code:
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; (setq org-ellipsis "⤵")

(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
(setq org-agenda-files '("~/Documents/org/"))


;;; custom-org.el ends here
