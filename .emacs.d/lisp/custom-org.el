;;; custom-org.el --- org mode config
;;;
;;; Commentary:
;;;
;;; org mode
;;;
;;; Code:

(use-package org
  :defer t
  :config
  ;;(setq org-agenda-files '("~/docs/org/"))
  (setq org-support-shift-select t)
  (setq org-return-follows-link t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-export-html-postamble nil)
  (setq org-startup-folded (quote overview))
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-export-with-toc t)
  (setq org-directory "~/docs/org/")
  ;; make images pretty
  (setq org-image-actual-width (/ (display-pixel-width) 3))
  :bind
  ("C-c C-c" . org-capture)
  (:map org-mode-map
        ("<M-return>" . org-return-newline-below)
        ("C-`" . org-open-at-point-plaintext)
        ("<C-delete>" . org-remove-link)
        ("<C-escape>" . org-mark-ring-goto)
        ("<return>" . bcm/org-return)
        ("C-c C-c" . mc/edit-lines)
        ("C-c r" . github-start-review-at-link)))

;; https://orgmode.org/worg/org-contrib/org-protocol.html#org9e2e3ac
(setq org-capture-templates
      `(("t" "To Do" entry (file+headline "~/docs/org/todo.org" "Unfiled")
         "* %?\n%T" :prepend t)
        ("n" "Note" entry (file+headline "~/docs/org/notes.org" "Notes")
         "* %?\n%T" :prepend t)
        ("j" "Journal Entry" entry (file+olp+datetree "~/docs/org/journal.org")
         "* %?\nTime: %U\n" :prepend t)
        ("l" "Link" entry (file+headline "~/docs/org/links.org" "Links")
        "* %? \n[[%:link][%:description]] \nCaptured On: %U")
        ("L" "Quote Link" entry (file+headline "~/docs/org/links.org" "Links")
         "* %? \nSource: %:link, \n\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\nCaptured On: %U")
        ))

;; overwrite web mode binding for C-c C-l
(global-set-key (kbd "C-c C-l") 'org-store-link)

;; start github review with given link at point
(defun github-start-review-at-link ()
  "Copies the URL from an org link at the point and starts github review"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (github-review-start plain-url)))))

;; (use-package org-modern
;;   :ensure t)





(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/kb/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      (file "~/.emacs.d/org-templates/default.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
     ("f" "fungi" plain
      (file "~/.emacs.d/org-templates/fungi.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
     ("p" "plant" plain
      (file "~/.emacs.d/org-templates/plant.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
     ))
  :bind(("C-c n f" . org-roam-node-find)
        ("C-c n i" . org-roam-node-insert)
        ("C-c n l" . org-roam-buffer-toggle)
        :map org-mode-map
        ("<backtab>" . completion-at-point)
        )
  :config
  (org-roam-setup)
  )

(require 'org-roam-protocol)






;; pretty bullets
;; (use-package org-bullets
;;   :defer t
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode))


(use-package ox-reveal
  :defer t
  :ensure ox-reveal
  )


(setq org-reveal-root
      ;; "https://cdnjs.com/libraries/reveal.js/3.6.0"
      "file:///home/leah/coding/reveal.js"
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
