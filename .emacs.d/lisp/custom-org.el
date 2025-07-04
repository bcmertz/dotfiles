;;; custom-org.el --- org mode config -*- lexical-binding: t -*-
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
  (setq org-hide-emphasis-markers t)
  (setq org-capture-bookmark nil)
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
        ("<return>" . my-org-return)
        ("C-c C-c" . mc/edit-lines)
        ("C-c r" . github-start-review-at-link)))

(use-package org-indent
  :straight nil
  :defer t)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG ROAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-roam
;;   :defer t
;;   :custom
;;   (org-roam-directory "~/kb/")
;;   (org-roam-completion-everywhere t)
;;   ;; more info on template syntax
;;   ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
;;   (org-roam-capture-templates
;;    '(("d" "default" plain
;;       (file "~/.emacs.d/org-templates/default.org")
;;       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
;;      ("f" "fungi" plain
;;       (file "~/.emacs.d/org-templates/fungi.org")
;;       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Fungus") :unnarrowed t)
;;      ("p" "plant" plain
;;       (file "~/.emacs.d/org-templates/plant.org")
;;       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Plant") :unnarrowed t)
;;      ("r" "Food" plain
;;       (file "~/.emacs.d/org-templates/food.org")
;;       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Food") :unnarrowed t)
;;      ))
;;   (org-roam-node-display-template
;;    (concat "${title:*} "
;;            (propertize "${tags:10}" 'face 'org-tag)))
;;   :bind(("C-c n f" . org-roam-node-find-immediate)
;;         ("C-c n i" . org-roam-node-insert-immediate)
;;         ("C-c n F" . org-roam-node-find)
;;         ("C-c n I" . org-roam-node-insert)
;;         ("C-c n l" . org-roam-buffer-toggle)
;;         :map org-mode-map
;;         ("<backtab>" . completion-at-point)
;;         )
;;   :init
;;   (require 'org-roam-protocol)
;;   (require 'org-roam-capture)
;;   :config
;;   (which-key-add-key-based-replacements "C-c n" "org-roam")

;;   ;; display tags in search results
;;   ;; (setq org-roam-node-display-template (concat "${title:40 " (propertize "${tags:*}" 'face 'org-tag)))
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))

;;   ;; fit org select buffer to the height of all the options
;;   (add-to-list 'display-buffer-alist
;;                '("^\\*Org Select*"
;;                  (display-buffer-in-direction)
;;                  (window-height . fit-window-to-buffer)))
;;   )

;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   "Create and insert roam node without switching to it."
;;   (interactive "P")
;;   (let ((args (cons arg args))
;;         (org-roam-capture-templates (mapcar (lambda (elem)
;;                                               (append elem '(:kill-buffer)))
;;                                             org-roam-capture-templates
;;                                             )))
;;     (apply #'org-roam-node-insert args)))

;; (defun org-roam-node-find-immediate (arg &rest args)
;;   "Create and insert roam node without switching to it."
;;   (interactive "P")
;;   (let ((args (cons arg args))
;;         (org-roam-capture-templates (mapcar (lambda (elem)
;;                                               (append elem '(:kill-buffer)))
;;                                             org-roam-capture-templates
;;                                             )))
;;     (apply #'org-roam-node-find args)))

;; (defun org-roam-filter-by-tag (tag-name)
;;   "Filter nodes by TAG-NAME."
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun org-roam-list-notes-by-tag (tag-name)
;;   "List nodes with given TAG-NAME."
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STYLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; pretty bullets
(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))


(add-hook 'org-mode-hook 'variable-pitch-mode)

;; make org preeeetty
(eval-after-load "org"
  '(add-hook 'org-mode-hook 'set-org-faces))
;; when we change the theme reload org faces
(add-hook 'after-load-them-hook 'set-org-faces)

(defun set-org-faces ()
  "Set org faces."
  ;; https://github.com/zzamboni/dot-emacs/blob/master/init.org
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; should be enabled by default, buffer-face-mode allows changing
  ;; the values of various face - see (describe-face) or (counsel-describe-face)
  ;; or type C-u C-x = to see what faces the selected text has
  ;; (buffer-face-mode t)

  (set-face-attribute 'org-default nil :inherit 'fixed-pitch :height 1.1) ;; base from which everything is scaled
  (set-face-attribute 'org-level-1 nil :weight 'medium :height 1.3)
  (set-face-attribute 'org-level-2 nil :weight 'medium :height 1.2)
  (set-face-attribute 'org-level-3 nil :weight 'medium :height 1.1)
  (set-face-attribute 'org-level-4 nil :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-5 nil :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-6 nil :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-7 nil :weight 'medium :height 1.05)
  (set-face-attribute 'org-level-8 nil :weight 'medium :height 1.05)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch) :height 1.0)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :height 1.0)
  (set-face-attribute 'org-block nil :inherit '(shadow fixed-pitch) :height 0.95)
  (set-face-attribute 'org-document-title nil :foreground "dark orange" :weight 'bold :height 1.75)
  (set-face-attribute 'org-document-info nil :foreground "dark orange" :height 1.3)
  (set-face-attribute 'org-link nil :foreground "royal blue" :underline t :height 1.0)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :foreground "#83a598")
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula t :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox t :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch) :weight 'bold :height 0.8)
  (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch) :height 0.9)

  ;; ;; Get rid of the background on column views
  ;; (set-face-attribute 'org-column nil :background nil)
  ;; (set-face-attribute 'org-column-title nil :background nil)
  )

;; styling modern
(use-package org-modern
  :defer t
  :config
  (setq org-modern-block t)
  (setq org-modern-tag t)
  (setq org-modern-horizontal-rule t)
  (setq org-modern-keyword t)
  (setq org-modern-list ())

  (setq org-modern-timestamp t)
  (setq org-modern-todo t)
  (setq org-modern-priority t)

  (setq org-modern-radio-target nil)
  (setq org-modern-table-vertical nil)
  (setq org-modern-internal-target nil)
  (setq org-modern-star nil)
  (setq org-modern-label-border nil)
  (setq org-modern-checkbox nil)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-hide-stars nil)
  (setq org-modern-progress nil)
  (setq org-modern-table nil)
  (setq org-modern-table-horizontal nil)
  (setq org-modern-statistics nil)
  :init
  (global-org-modern-mode)
  )

;; ;; table of contents - maybe add save hook in org mode
;; (use-package org-make-toc
;;   :hook (org-mode . org-make-toc-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRESENTATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package org-present
  :defer t)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-hide-cursor)
                 ;; (org-present-read-only)
                 ;; (org-present-big)
                 (setq-local face-remapping-alist '(
                                                    (default (:height 1.5) variable-pitch)
                                                    (header-line (:height 4.0) variable-pitch)
                                                    (org-document-title (:height 1.75) org-document-title)
                                                    (org-code (:height 1.55) org-code)
                                                    (org-verbatim (:height 1.55) org-verbatim)
                                                    (org-block (:height 1.25) org-block)
                                                    (org-block-begin-line (:height 0.7) org-block)))
                 (global-hl-line-mode -1)
                 (turn-on-hide-mode-line-mode)
                 (setq header-line-format " ")
                 (org-display-inline-images)
                 (org-overview)
                 (org-show-entry)
                 (org-show-children)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-show-cursor)
                 ;; (org-present-read-write)
                 ;; (org-present-small)
                 (setq-local face-remapping-alist '((default default default)))
                 (turn-off-hide-mode-line-mode)
                 (setq header-line-format nil)
                 (setq buffer-face-mode-face nil)
                 (org-remove-inline-images)))))

;; ;; use org-present instead
;; (use-package ox-reveal
;;   :defer t
;;   :straight ox-reveal
;;   )
;; (setq org-reveal-root
;;       ;; "https://cdnjs.com/libraries/reveal.js/3.6.0"
;;       "file:///home/reya/coding/reveal.js"
;;       )
;; (setq org-reveal-mathjax t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PUBLISHING ORG->HTML ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OUT OF DATE
;; follow https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/
;; or https://www.youtube.com/watch?v=AfkrzFodoNw
;; to get this working again

;; (use-package htmlize
;;   :defer t
;;   )
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

(provide 'custom-org)
;;; custom-org.el ends here
