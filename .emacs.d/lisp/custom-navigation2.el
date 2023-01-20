;;; custom-navigation2.el --- navigation2 configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-multiform
                                vertico-directory))
  :init (vertico-mode)
  :bind (:map vertico-map
         ("<next>" . vertico-scroll-up)
         ("<prior>" . vertico-scroll-down))
  :config
  (setq vertico-preselect 'first))

(defun sort-directories-first (files)
  "Sort directories before FILES."
  (setq files (vertico-sort-history-length-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(use-package vertico-multiform
  :after vertico
  :straight nil
  :init (vertico-multiform-mode)
  :config
  ;; magit should not preselect so we can by default select directory
  ;; no posframe for isearch equivalent so we dont hide text
  (setq vertico-multiform-commands
        '((magit-status posframe (vertico-preselect . prompt))
          (consult-line (:not posframe))))
  ;; find file bind / to directory awareness
  ;; default posframe, with / unbound
  (setq vertico-multiform-categories
        '((file (lambda (_) (define-key vertico-map "/" #'vertico-directory-enter)) posframe)
          (consult-grep buffer)
          (t posframe (lambda (_) (define-key vertico-map "/" #'self-insert-command)))))
  ;; sort directories first
  (setq vertico-sort-override-function 'sort-directories-first))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(defun my-vertico-posframe-get-size (buffer)
  "Set the vertico-posframe size according to the current frame."
  (let* ((height (or vertico-posframe-height 10))
         (min-height (min height (+ 1 (length vertico--candidates))))
         (width (min (or vertico-posframe-width 200) (round (* .75 (frame-width))))))
    (list
     :height height
     :width width
     :min-height min-height
     :min-width width)))

(use-package vertico-posframe
  :after vertico
  :init
  (apply-if-gui 'vertico-posframe-mode)
  :config
  (add-hook 'vertico-posframe-mode-hook 'vertico-posframe-cleanup)
  (setq vertico-posframe-size-function 'my-vertico-posframe-get-size))

;; apply contrast styling
(defun my-posframe-tweaks (&optional frame)
  "My posframe tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (set-face-attribute 'vertico-posframe nil :background (get-theme-variable-from-palette 'bg-alt)))

(add-hook 'after-make-frame-functions #'my-posframe-tweaks t)
(add-hook 'after-load-theme-hook #'my-posframe-tweaks t)
(my-posframe-tweaks)



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
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s s" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("C-s" . consult-line)                  ;; needed by consult-line to detect isearch
         ("C-r" . consult-line)
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  ;; (setq register-preview-delay 0.5
        ;; register-preview-function #'consult-register-format)

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
  (setq consult-preview-key nil)
  (consult-customize consult-line :preview-key 'any)
  (consult-customize consult-theme :preview-key (kbd "M-<return>"))
  (consult-customize my-consult-change-theme :preview-key (kbd "M-<return>"))

  ;; TODO figure out ordering error on startup putting recent files first
  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-modified-buffer
                                 consult--source-buffer
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-bookmark
                                 consult--source-project-buffer
                                 consult--source-project-recent-file))
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  ;; :preview-key (kbd "M-.")
  ;;  :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

;; (use-package consult-ag
;;   :bind
;;   ("M-s" . consult-ag))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))


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
  :straight t

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)


;; nice to have in the future for cleaning up recent file suggestions
(use-package recentf
  :defer 1
  :straight nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/straight/.*" (getenv "HOME")))
  (recentf-mode +1))

;; project navigation
(defun projectile-find-file-refresh-cache ()
  "Projectile find file and invalidate cache."
  (interactive "")
  (projectile-invalidate-cache (if (current-project) nil t))
  (projectile-find-file)
  )

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'auto)
  (setq projectile-mode-line-prefix " ")
  (setq projectile-project-search-path '("~/coding/"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  ;; (setq projectile-indexing-method 'native)
  (which-key-add-key-based-replacements "C-c p P" "Projectile rediscover projects")
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
              ("C-c p F" . projectile-find-file-refresh-cache)
              ("C-c p P" . (lambda () (interactive)
                           (projectile-cleanup-known-projects)
                           (projectile-discover-projects-in-search-path)))
              ("C-c p p" . projectile-switch-project)
              ("C-c p b" . projectile-switch-to-buffer)
              ("C-c p f" . projectile-find-file)))
              ;; ("C-c p p" . consult-projectile-switch-project)
              ;; ("C-c p b" . consult-projectile-switch-to-buffer)
              ;; ("C-c p f" . consult-projectile-find-file)))

;; (use-package consult-projectile
  ;; :after consult
  ;; :after projectile
  ;; :config
  ;; (setq consult-projectile-use-projectile-switch-project t)
  ;; )


;; better C-x C-b
(defalias 'list-buffers 'ibuffer)
(bind-key "q" 'kill-current-buffer 'ibuffer-mode-map)

;;; custom-navigation2.el ends here
