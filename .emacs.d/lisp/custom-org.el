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
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-export-with-toc t)
  (setq org-directory "~/docs/org/")
  ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STYLING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; pretty bullets
(use-package org-bullets
  :defer t
  :ensure t
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
  :ensure t)

(global-org-modern-mode)

;; ;; table of contents - maybe add save hook in org mode
;; (use-package org-make-toc
;;   :hook (org-mode . org-make-toc-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG ROAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
        ("C-c n I" . org-roam-node-insert-immediate)
        ("C-c n i" . org-roam-node-insert)
        ("C-c n l" . org-roam-buffer-toggle)
        :map org-mode-map
        ("<backtab>" . completion-at-point)
        )
  :config
  (org-roam-setup)
  )

(require 'org-roam-protocol)

;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRESENTATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package org-present :ensure t)

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
;;   :ensure ox-reveal
;;   )
;; (setq org-reveal-root
;;       ;; "https://cdnjs.com/libraries/reveal.js/3.6.0"
;;       "file:///home/leah/coding/reveal.js"
;;       )
;; (setq org-reveal-mathjax t)


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
