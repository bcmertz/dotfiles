;;; custom-git.el --- git/magit config
;;;
;;; Commentary:
;;;
;;; version control stuff
;;;
;;; Code:

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout)
  ("C-x l" . magit-log-branches))

(use-package github-review
  :defer t
  :after magit
  :bind (:map magit-mode-map
         ("C-c r" . github-review-forge-pr-at-point)))

(use-package forge
  :defer t
  :after magit)

(use-package git-link
  :defer t
  :bind
  ("C-c l" . git-link))

(use-package diffview
  :defer t
  :mode "\\.diff\\'"
  )

;;; custom-git.el ends here
