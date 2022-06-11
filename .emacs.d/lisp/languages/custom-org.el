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
  :defer t
  :config
  (setq org-support-shift-select t)
  :bind (:map org-mode-map
              ("<M-return>" . org-return-newline-below)
              ("C-`" . org-open-at-point-plaintext)
              ("<C-delete>" . org-remove-link)
              ("<C-escape>" . org-mark-ring-goto)
              ("<return>" . bcm/org-return)
              ("C-c r" . github-start-review-at-link)
              ))

(use-package org-drill
  :defer t
  :ensure t
  )

;; brokne for some reason
;; (add-to-list 'org-structure-template-alist
;;              '("s" "#+BEGIN_SRC ?\n\n#+END_SRC"))

;; pretty bullets
(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follows-link t)
;;(setq org-agenda-files '("~/docs/org/"))

;; overwrite web mode binding for C-c C-l
(add-hook 'web-mode-hook
      (lambda ()
        (local-unset-key (kbd "C-c C-l"))))
(global-set-key (kbd "C-c C-l") 'org-store-link)

;; make images pretty
(setq org-image-actual-width (/ (display-pixel-width) 3))

(use-package ox-reveal
  :defer t
  :ensure ox-reveal
  )

(custom-set-variables
 '(org-export-html-postamble nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded (quote overview))
 '(org-startup-indented t)
 '(org-confirm-babel-evaluate nil)
 '(org-src-fontify-natively t)
 '(org-export-with-toc t)
 )

(setq org-reveal-root
      ;; "https://cdnjs.com/libraries/reveal.js/3.6.0"
      "reveal.js"
      )
(setq org-reveal-mathjax t)

(use-package htmlize
  :defer t
  :ensure t
  )


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

;;; custom-org.el ends here
