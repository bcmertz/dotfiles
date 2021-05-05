;;; custom-autocomplete.el --- autocomplete configuration
;;;
;;; Commentary:
;;;
;;; autocompletion with company mode
;;;
;;; Code:
(use-package company
  :defer t
  :after eldoc
  :init
  (progn
    (global-company-mode)
    (setq company-tooltip-limit 20)                         ; bigger popup window
    (setq company-idle-delay nil)                           ; don't autocomplete on typing, backtab instead
    ;; (setq company-idle-delay .3)  			     ; decrease delay before autocompletion popup shows
    ;;(setq company-echo-delay 0)                           ; remove annoying blinking
    ;;(setq company-begin-commands '(self-insert-command))  ; start autocompletion after typing, if we want to ignore our special tab key we bind below
    ))


(use-package company-box
  :ensure t
  :defer t
  :hook
  (company-mode . company-box-mode))

;; (global-set-key [tab] 'tab-indent-or-complete)
(global-set-key [backtab] 'company-complete-common) ;; backtab triggers autocomplete

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

(use-package yasnippet
  :defer t
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/sh-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/web-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/org-mode")
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/emacs-lisp-mode")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (yas-global-mode 1)
  )

;;; custom-autocomplete.el ends here
