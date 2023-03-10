;;; custom-navigation.el --- navigation configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
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
  (setq vertico-preselect 'first)
  :hook
  (minibuffer-setup . (lambda () (interactive) (pixel-scroll-precision-mode -1)))
  (minibuffer-exit . (lambda () (interactive) (pixel-scroll-precision-mode 1)))
  )

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
  ;; consult-line cycles and in minibuffer so we don't hide text
  (setq vertico-multiform-commands
        '((magit-status posframe (vertico-preselect . prompt))
          (consult-line (:not posframe) (vertico-cycle . t))))
  ;; all consult grep like commands (grep / ripgrep / git grep) take place in dedicated buffer
  ;; find file bind / to directory awareness
  ;; default posframe, with / unbound
  (setq vertico-multiform-categories
        '(;; (consult-grep buffer)
          (file posframe (lambda (_) (progn
                              (define-key vertico-map "/" #'vertico-directory-enter)
                              (setq-local vertico-sort-override-function 'sort-directories-first))))
          (t posframe
             (vertico-posframe-fallback-mode . vertico-mode-buffer)
             (lambda (_) (define-key vertico-map "/" #'self-insert-command))
             ))))

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
  (vertico-posframe-mode)
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

(defun my-consult-locate () "My consult-locate." (interactive "") (consult-locate (choose-directory)))
(defun my-consult-grep () "My consult-grep." (interactive "") (consult-grep (choose-directory)))
(defun my-consult-git-grep () "My consult-git-grep." (interactive "") (consult-git-grep (choose-directory)))
(defun my-consult-ripgrep () "My consult-ripgrep." (interactive "") (consult-ripgrep (choose-directory)))
(defun my-consult-find () "My consult-find." (interactive "") (consult-find (choose-directory)))

(use-package consult
  :commands (consult--read)
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
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . my-consult-find)
         ("M-s D" . my-consult-locate)
         ("M-s g" . my-consult-grep)
         ("M-s G" . my-consult-git-grep)
         ("M-s s" . my-consult-ripgrep)
         ("M-s l" . consult-line-multi)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ("C-S-s" . consult-line-multi)
         ("C-s-r" . consult-line-multi)
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
  (consult-customize consult-theme :preview-key "C-<return>")
  (consult-customize my-consult-change-theme :preview-key "C-<return>")
  (setq consult-line-start-from-top nil)
  (consult-customize consult-buffer :history nil)

  (setq consult-buffer-filter '("\\` " "\\`\\*Completions\\*\\'"
                                "\\`\\*Flymake log\\*\\'"
                                "\\`\\*Semantic SymRef\\*\\'"
                                "\\`\\*tramp/.*\\*\\'"
                                "\\`\\*straight-process\\*\\'"
                                "\\`\\*Async-native-compile-log\\*\\'"
                                ))
  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 consult--source-modified-buffer
                                 consult--source-buffer
                                 consult--source-recent-file
                                 consult--source-file-register
                                 consult--source-bookmark
                                 consult--source-project-buffer
                                 consult--source-project-recent-file))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

;; (defun my-consult-ag () "My consult-ag." (interactive "") (consult-ag (choose-directory)))

;; (use-package consult-ag
;;   :defer t
;;   :bind
;;   ("M-s a" . my-consult-ag))

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
  :init
  (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map vertico-map (
         ("C-." . embark-act)         ;; pick some comfortable binding
         ("M-." . embark-dwim)
         ("M-<return>" . embark-collect)
         ))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(defun embark-collect-no-read-only ()
  "Ignore attempts to make embark-collect read-only by default."
  (setq-local inhibit-read-only t))
(add-hook 'embark-collect-mode-hook 'embark-collect-no-read-only)

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
  :defer 0.1
  :straight nil
  :config
  ;; TODO recentf-exclude not working
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/straight/.*" (getenv "HOME")))
  (recentf-mode +1))

;; better C-x C-b
(use-package ibuffer
  :defer t
  :bind (("C-x C-b" . ibuffer))
  :config
  (bind-key "q" 'kill-current-buffer 'ibuffer-mode-map)
  (defalias 'list-buffers 'ibuffer))

;; code navigation
(use-package avy
  :bind (("M-g" . avy-goto-char)    ;; go to char
         ("M-l" . avy-goto-line)))  ;; go to line

(provide 'custom-navigation)
;;; custom-navigation.el ends here
