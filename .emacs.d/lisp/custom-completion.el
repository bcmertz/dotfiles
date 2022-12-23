;;; custom-completion.el --- completion configuration
;;;
;;; Commentary:
;;;
;;; completion, documentation, and yasnippet
;;;
;;; Code:

(setq completion-ignore-case t)
(setq tab-always-indent t)
(global-set-key [backtab] 'completion-at-point) ;; backtab triggers completion

;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;

(use-package corfu
  :after eldoc
  :config
  (setq corfu-separator ?\s)             ;; Orderless field separator - space
  (setq corfu-popupinfo-delay 0.2)
  :bind
  ;; Configure SPC for separator insertion, plays nicely with orderless for completion
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (add-to-list 'load-path
               (expand-file-name "straight/build/corfu/extensions"
                                 straight-base-dir))
  (require 'corfu-popupinfo)
  (require 'corfu-history)

  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (eldoc-add-command #'corfu-insert)
  )


;;;;;;;;;;;; builtin arglist and variable docstrings info in echo area ;;;;;;;;;;;;;;

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.2)
  :init
  (global-eldoc-mode)
  )


;;;;;;;;;;;;;;;;;;;; Completion styling ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; orderless completion style - allow space separated `search terms'
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))



;;;;;;;;;;;;;;;;;;;;; syntax highlighting ;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; turn on tree-sitter only for programming modes - breaks for some reason
;; (defun turn-on-tree-sitter ()
;;   "Turn on tree-sitter."
;;   (tree-sitter-mode 1)
;;   )
;; (add-hook 'prog-mode-hook #'turn-on-tree-sitter)
(global-tree-sitter-mode)



;; always turn on tree-sitter-hl-mode
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; prevent bolding function calls when the cursor isn't on them
(set-face-attribute 'tree-sitter-hl-face:function.call nil :weight 'normal)


;;;;;;;;;;;;;;; snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :defer t
  :init
  (yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("C-c &" . nil)
        ("C-c y i" . yas-insert-snippet)
        ("C-c y n" . yas-new-snippet)
        ("C-c y v" . yas-visit-snippet-file))
  :config
  (which-key-add-key-based-replacements "C-c y" "yasnippet")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/sh-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/web-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/org-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/emacs-lisp-mode")
  (yas-global-mode 1)
  )



;;;;;;;;;;;;;;;;;;;;; DEPRECATED - replaced by corfu

;; (use-package company
;;   :defer t
;;   :after eldoc
;;   :init
;;   (progn
;;     (global-company-mode)
;;     (setq company-tooltip-limit 20)                         ; bigger popup window
;;     (setq company-idle-delay nil)                           ; don't completion on typing, backtab instead
;;     ;; (setq company-idle-delay .3)  			     ; decrease delay before completion popup shows
;;     ;;(setq company-echo-delay 0)                           ; remove annoying blinking
;;     ;;(setq company-begin-commands '(self-insert-command))  ; start completion after typing, if we want to ignore our special tab key we bind below
;;     ))


;; (use-package company-box
;;   :defer t
;;   :hook
;;   (company-mode . company-box-mode))

;; ;; (global-set-key [tab] 'tab-indent-or-complete)
;; (global-set-key [backtab] 'company-complete-common) ;; backtab triggers completion

;; useful if using tab for expanding completion and indent
;; https://www.emacswiki.org/emacs/CompanyMode#toc11
;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))




;;; custom-completion.el ends here
