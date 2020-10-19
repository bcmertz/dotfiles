;;; custom-org.el --- org mode config
;;;
;;; Commentary:
;;;
;;; org mode
;;;
;;; Code:
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; start github review with given link at point
(defun github-start-review-at-link ()
  "Copies the URL from an org link at the point and starts github review"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (github-review-start plain-url)))))

(use-package org
  :bind (:map org-mode-map
         ("C-c r" . github-start-review-at-link)))

;; pretty bullets
(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
(setq org-agenda-files '("~/Documents/org/"))


;; overwrite web mode binding for C-c C-l
(add-hook 'web-mode-hook
      (lambda ()
        (local-unset-key (kbd "C-c C-l"))))
(global-set-key (kbd "C-c C-l") 'org-store-link)

;;; custom-org.el ends here
