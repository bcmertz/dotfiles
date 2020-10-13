;;; custom-git.el --- git/magit config
;;;
;;; Commentary:
;;;
;;; version control stuff
;;;
;;; Code:

(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout)
  ("C-x l" . magit-log-branches))

(use-package github-review
  :defer t
  :bind
  ("C-c r" . github-review-forge-pr-at-point))

(use-package forge
  :defer t
  :after magit)

(use-package git-link
  :defer t
  :bind
  ("C-c l" . git-link))

;;; custom-git.el ends here
