;;; custom-org.el --- org mode config
;;;
;;; Commentary:
;;;
;;; org mode
;;;
;;; Code:
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELED")))

(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t)

;;; custom-org.el ends here
