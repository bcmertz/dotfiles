;;; custom-project.el --- custom project
;;;
;;; Commentary:
;;;
;;; Code:

;; TODO figure out how to do recentf ordering
(use-package project
  :defer t
  :config
  (global-set-key (kbd "C-c p") project-prefix-map)
  (which-key-add-key-based-replacements "C-c p" "project")
  (global-set-key (kbd "C-c p t") 'vterm-toggle)
  (global-set-key (kbd "C-c p f") 'consult-project-extra-find)
  (setq project-vc-extra-root-markers '(".projectile" "requirements.txt" "package.json" "go.mod"))
  (setq project-vc-ignores '("ido.last" "eln-cache/" ".cache/" ".saves/" "save-perspective"
                             "elpa/" "straight/" "auto-save-list/" "undo/" "var/" "tramp"
                             ".lsp-session-v1" "history" "org-roam.db" "multisession/"
                             "vimish-fold/" "transient/" "tree-sitter/" "Clementine/"
                             "Code - OSS/" "Element/" "Pulsar/" "Signal*/" "Chromium/"
                             "akonadi*" "balena-etcher/" "cef_user_data/" "chromium/"
                             "icedtea-web/" "libreoffice/" "whatsdesk/"
                             ))
  ;; always use consult-project-buffer instead of prompting different switching options
  ;; (setq project-switch-commands 'consult-project-buffer)
  (setq project-switch-commands 'consult-project-extra-find)
  ;; alternatively use multiple switching commands
  ;; (add-to-list 'project-switch-commands '(vterm-toggle "vterm" ?t))
  )

;; TODO remove "... (choose a dir)" with advice or something
;; (advice-add 'project-prompt-project-dir :before '((dir-choice . "testtttt")))
;; (defun my-project-prompt-project-dir (orig-fun &rest args)
;;   "This is ORIG-FUN and ARGS."
;;   (let ((dir-choice "testttttttttt")
;;         (res (apply orig-fun args)))
;;     res))
;; (defun my-project-prompt-project-dir ()
;;   "Prompt the user for a directory that is one of the known project roots.
;; The project is chosen among projects known from the project list,
;; see `project-list-file'.
;; It's also possible to enter an arbitrary directory not in the list."
;;   (project--ensure-read-project-list)
;;   (let* ((choices
;;           (project--file-completion-table
;;            project--list))
;;          (pr-dir ""))
;;     (while (equal pr-dir "")
;;       ;; If the user simply pressed RET, do this again until they don't.
;;       (setq pr-dir (completing-read "Select project: " choices nil t)))
;;     (if (equal pr-dir dir-choice)
;;         (read-directory-name "Select directory: " default-directory nil t)
;;       pr-dir)))
;; (advice-add 'project-prompt-project-dir :override #'my-project-prompt-project-dir)



(use-package consult-project-extra
  :defer t
  :bind
  (("C-c p f" . consult-project-extra-find)))

;; (use-package consult-dir
;;   :defer t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map minibuffer-local-completion-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))

;; ;; project navigation
;; (defun projectile-find-file-refresh-cache ()
;;   "Projectile find file and invalidate cache."
;;   (interactive "")
;;   (projectile-invalidate-cache (if (current-project) nil t))
;;   (projectile-find-file)
;;   )

;; (use-package projectile
;;   :defer t
;;   :init
;;   (projectile-mode +1)
;;   :config
;;   (setq projectile-sort-order 'recentf)
;;   (setq projectile-completion-system 'auto)
;;   (setq projectile-mode-line-prefix " ")
;;   (setq projectile-project-search-path '("~/coding/"))
;;   (setq projectile-enable-caching t)
;;   (setq projectile-indexing-method 'hybrid)
;;   ;; (setq projectile-indexing-method 'native)
;;   (which-key-add-key-based-replacements "C-c p P" "Projectile rediscover projects")
;;   :bind (:map projectile-mode-map
;;               ("s-p" . projectile-command-map)
;;               ("C-c p" . projectile-command-map)
;;               ("C-c p F" . projectile-find-file-refresh-cache)
;;               ("C-c p P" . (lambda () (interactive)
;;                            (projectile-cleanup-known-projects)
;;                            (projectile-discover-projects-in-search-path)))
;;               ("C-c p p" . projectile-switch-project)
;;               ("C-c p b" . projectile-switch-to-buffer)
;;               ("C-c p f" . projectile-find-file)))
;;               ;; ("C-c p p" . consult-projectile-switch-project)
;;               ;; ("C-c p b" . consult-projectile-switch-to-buffer)
;;               ;; ("C-c p f" . consult-projectile-find-file)))

;; (use-package consult-projectile
;;   :after consult
;;   :after projectile
;;   :config
;;   (setq consult-projectile-use-projectile-switch-project t)
;;   )

;; perspective navigation
;; (use-package perspective
;;   :bind (
;;          ("C-x B" . persp-counsel-switch-buffer)
;;          ;; ("C-x k" . persp-kill-buffer*)
;;          ("C-M-<left>" . persp-prev)
;;          ("C-M-<right>" . persp-next)
;;          ("C-M-<return>" . persp-switch)
;;          ("C-M-<delete>" . persp-state-save)
;;          ("C-M-<backspace>" . persp-state-load)
;;          )
;;   :init
;;   (setq persp-initial-frame-name "main")
;;   (setq persp-sort 'created)
;;   (persp-mode)
;;   :config
;;   ;; dont show modeline string
;;   (setq persp-show-modestring nil)
;;   ;; TODO figure out why restoring expects a directory
;;   (setq persp-state-default-file "~/.emacs.d/save-perspective")
;;   (add-hook 'kill-emacs-hook #'persp-state-save)
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-c M-p"))
;;   (persp-add-buffer-to-frame-global "*Messages*")
;;   )

;; (defun my-show-persp-modestring ()
;;   "Show persp modestring."
;;   (if (< 1 (length (persp-names)))
;;       (setq persp-show-modestring t)
;;     (setq persp-show-modestring nil)
;;     )
;;   )

;; (add-hook 'persp-created-hook #'my-show-persp-modestring)
;; (add-hook 'persp-killed-hook #'my-show-persp-modestring)

(provide 'custom-project)
;;; custom-project.el ends here
