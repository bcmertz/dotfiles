;;; custom-completion.el --- completion configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; completion, documentation, and yasnippet
;;;
;;; Code:

(setq completion-ignore-case t)
(setq tab-always-indent t)
(global-set-key [backtab] 'completion-at-point) ;; backtab triggers completion

;;;;;;;;;;;;;;;;;;;;;;;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :after eldoc
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo
                              corfu-info
                              corfu-history))
  :config
  (setq corfu-separator ?\s)             ;; Orderless field separator - space
  (setq corfu-popupinfo-delay 0.2)
  (setq corfu-preview-current nil)       ;; don't fill in selection cursor is on until enter
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :bind
  ;; Configure SPC for separator insertion, plays nicely with orderless for completion
  (:map corfu-map (("SPC" . corfu-insert-separator)
                   ("M-g" . corfu-info-location)
                   ("M-h" . corfu-info-documentation)
                   ("M-<return>" . corfu-move-to-minibuffer)
                   ("<next>" . corfu-scroll-up)
                   ("<prior>" . corfu-scroll-down)))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (eldoc-add-command #'corfu-insert)
  )

(if-gui nil
        (use-package corfu-terminal
          :init
          (corfu-terminal-mode +1)))

;;;;;;;;;;;;;;;;;;;; Completion styling ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; orderless completion style - allow space separated `search terms'
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;;;;;;;;;;;; builtin arglist and variable docstrings info in echo area ;;;;;;;;;;;;;;

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.2)
  :init
  (global-eldoc-mode)
  )



;;;;;;;;;;;;;;;;;;;;;;;;; Snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :defer t
  :init
  (advice-add 'yas-reload-all :around #'silence-function-messages) ;; remove yas messages while reloading
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


(provide 'custom-completion)
;;; custom-completion.el ends here
