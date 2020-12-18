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
         ("<M-return>" . org-return-newline-below)
         ("C-c r" . github-start-review-at-link)))

;; failed attempts to add front matter to exported html doc
;; (setq org-publish-project-alist
;; '(("org-bcmertz"
;;    ;; Path to your org files.
;;    :base-directory "~/docs/bcmertz.github.io/"
;;    :base-extension "org"

;;    ;; Path to your Jekyll project.
;;    :publishing-directory "~/docs/bcmertz.github.io/"
;;    :recursive t
;;    :publishing-function org-publish-org-to-html
;;    :headline-levels 4
;;    :html-extension "html"
;;    :body-only t ;; Only export section between <body> </body>
;;    )))
;; (setq org-publish-project-alist
;;       '(("org-portfolio"
;;          :base-directory "~/docs/bcmertz.github.io/"
;;          :base-extendion "org"
;;          :publishing-directory "~/docs/bcmertz.github.io/"
;;          :recursive t
;;          :makeindex t
;;          :html-preamble "This is just a test"
;;          :publishing-function org-html-publish-to-html)
;;         ("org" :components ("org-notes"))))

;; pretty bullets
(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
(setq org-agenda-files '("~/docs/org/"))


;; overwrite web mode binding for C-c C-l
(add-hook 'web-mode-hook
      (lambda ()
        (local-unset-key (kbd "C-c C-l"))))
(global-set-key (kbd "C-c C-l") 'org-store-link)

;; make images pretty
(setq org-image-actual-width (/ (display-pixel-width) 3))

;;; custom-org.el ends here
