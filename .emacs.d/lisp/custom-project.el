;;; custom-project.el --- custom project -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package project
  :defer t
  :config
  (global-set-key (kbd "C-c p") project-prefix-map)
  (which-key-add-key-based-replacements "C-c p" "project")
  (global-set-key (kbd "C-c p t") 'vterm-toggle)
  (global-set-key (kbd "C-c p f") 'consult-project-extra-find)
  (setq project-vc-extra-root-markers '(".projectile" "requirements.txt" "package.json" "go.mod" "README.md" "init.el"))
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

;; this creates consult like completion within projects
;;
;; known limitation: recentf ordering not possible
;; https://github.com/Qkessler/consult-project-extra/issues/10
(use-package consult-project-extra
  :defer t
  :custom (consult-project-function #'consult-project-extra-project-fn) ;; Optional but recommended for a more consistent UI
  :bind
  (("C-c p f" . consult-project-extra-find)))

;; (use-package consult-dir
;;   :defer t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map minibuffer-local-completion-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))

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
;;   ;; figure out why restoring expects a directory
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
