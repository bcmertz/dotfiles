;;; custom-navigation2.el --- navigation2 configuration
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("C-s" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

(use-package consult-ag
  :bind
  ("M-s" . consult-ag))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package vertico-posframe)
(defun start-vertico-posframe ()
  "Start vertico-posframe."
  (vertico-posframe-mode))
(apply-if-gui 'start-vertico-posframe)

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; nice to have in the future for cleaning up recent file suggestions
(use-package recentf
  :defer 1
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/straight/.*" (getenv "HOME")))
  (recentf-mode +1))

;; project navigation
(defun projectile-find-file-refresh-cache ()
  "Projectile find file and invalidate cache."
  (interactive "")
  (projectile-find-file t)
  )

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'auto)
  ;; (setq projectile-indexing-method 'hybrid)
  (setq projectile-mode-line-prefix " ")
  (setq projectile-project-search-path '("~/coding/"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  (which-key-add-key-based-replacements "C-c p P" "Projectile rediscover projects")
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c p F" . projectile-find-file-refresh-cache)
              ("C-c p P" . (lambda () (interactive)
                           (projectile-cleanup-known-projects)
                           (projectile-discover-projects-in-search-path)))))

;; Sidebar File navigation
(use-package neotree ;; C-c C-c makes the focused directory the new root view
  :config
  (setq-default neo-show-hidden-files t) ;; Type H to toggle hidden files
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-mode-line-type 'none
        neo-show-updir-line nil
        neo-autorefresh nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-width 30
        neo-window-fixed-size ())
  :bind ("C-\\" . neotree-project-dir-toggle)
  :custom-face
  (neo-dir-link-face  ((t (:inherit fixed-pitch))))
  (neo-header-face    ((t (:inherit fixed-pitch))))
  (neo-banner-face    ((t (:inherit fixed-pitch))))
  (neo-root-dir-face  ((t (:inherit fixed-pitch))))
  (neo-file-link-face ((t (:inherit fixed-pitch))))
  )

(defun set-neotree-styling ()
  "Function to style neotree buffer locally."
  (add-hook 'neotree-mode-hook
	    (lambda ()
              ;; remove margins
              (setq-local left-margin-width 0 right-margin-width 0)
              (set-window-buffer nil (current-buffer))

              ;; no modeline
	      (setq mode-line-format nil)

              ;; enable variable pitch fonts
	      (buffer-face-mode 1)

	      ;; ;; locally change the buffer background color
	      ;; (setq buffer-face-mode-face `(:background "#21252B"))
	      ;; ;; if we want a fringe set it to a nice color
	      ;; ;; but only do it locally in neotree buffers
	      ;; (face-remap-add-relative 'fringe nil
	      ;;   		       '(:background "#21252B"))
	      ;; ;; title on neotree
	      ;; (set-face-attribute 'neo-root-dir-face nil
	      ;;   	          :box nil
	      ;;   	          ;; (:line-width 4 :color #21252B) doesn't work for some reason
	      ;;   	          :background "#21252B")

              ;; hl line background locally
	      ;; (face-remap-add-relative 'hl-line nil
	      ;;   	          :background "#353645") ;; #353645 gray ;; #4C77CB blue
              )))

;; apply if gui our neotree styling
(apply-if-gui 'set-neotree-styling)

;; The cursor always sits at bol. `+neotree--fix-cursor-h' and
;; `+neotree--indent-cursor-a' change that behavior so that the cursor is
;; always on the first non-blank character on the line, in the neo buffer. (src - doom-emacs)
(add-hook! 'neo-enter-hook
  (defun +neotree-fix-cursor-h (&rest _)
    (with-current-buffer neo-global--buffer
      (+neotree--indent-cursor-a))))

(defadvice! +neotree--indent-cursor-a (&rest _)
  :after '(neotree-next-line neotree-previous-line)
  (beginning-of-line)
  (skip-chars-forward " \t\r"))

;;; custom-navigation2.el ends here