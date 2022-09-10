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

(defun ~/magit-process-environment (env)
  "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel) and
https://github.com/magit/magit/issues/460#issuecomment-837449105."
  (let ((default (file-name-as-directory (expand-file-name default-directory)))
        (home (expand-file-name "~/")))
    (when (string= default home)
      (let ((gitdir (expand-file-name "~/.cfg/")))
        (push (format "GIT_WORK_TREE=%s" home) env)
        (push (format "GIT_DIR=%s" gitdir) env))))
  env)

(advice-add 'magit-process-environment
            :filter-return #'~/magit-process-environment)

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

;;; custom-git.el ends here
