;;; custom-git.el --- git/magit config
;;;
;;; Commentary:
;;;
;;; version control stuff
;;;
;;; Code:

;; Git integration

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x c") 'magit-checkout)

(use-package git-link
  :bind
  ("C-c l" . git-link)
  )

;;; custom-git.el ends here