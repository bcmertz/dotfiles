;;; custom-error-check.el --- syntax checking -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; syntax checking
;;;
;;; Code:

;;; TODO / NOTE: flycheck doesn't play nicely with eglot, so just flymake instead
;;; can check back later if eglot likes flycheck or maybe just
;;; use builtin flymake if there's not a compelling reason to use
;;; a non-builtin front-end for error checking

(use-package flymake
  :defer t
  :init
  (add-hook 'prog-mode-hook #'flymake-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'flymake-mode nil)
  :config
  (which-key-add-key-based-replacements "C-c !" "flymake")
  (global-set-key (kbd "C-c ! n") 'flymake-goto-next-error)
  (global-set-key (kbd "C-c ! p") 'flymake-goto-prev-error)
  (global-set-key (kbd "C-c ! s") 'flymake-start)
  (global-set-key (kbd "C-c ! i") 'flymake-show-diagnostic)
  (global-set-key (kbd "C-c ! l") 'flymake-show-buffer-diagnostics)
  (global-set-key (kbd "C-c ! m") 'flymake-mode)
  )

;; (use-package flycheck
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'after-init-hook #'global-flycheck-mode)
;;     ;; (defun setup-js-mode ()
;;     ;;   (flycheck-select-checker 'javascript-eslint))
;;     ;; (add-hook 'js-mode-hook #'setup-js-mode)
;;     )
;;   :config
;;   (flycheck-add-mode 'typescript-tslint 'web-mode)

;;   (setq flycheck-temp-prefix ".tmp")

;;   (setq-default flycheck-check-syntax-automatically '(save))
;;   ;; disable documentation related emacs lisp checker
;;   (setq-default flycheck-disabled-checkers '(go-staticcheck))
;;   (setq flycheck-mode-line-prefix "✔")

;;   (which-key-add-key-based-replacements "C-c !" "flycheck"))

;; from https://gist.github.com/purcell/ca33abbea9a98bb0f8a04d790a0cadcd
;; (defvar-local flycheck-eglot-current-errors nil)

;; (defun flycheck-eglot-report-fn (diags &rest _)
;;   (setq flycheck-eglot-current-errors
;;         (mapcar (lambda (diag)
;;                   (save-excursion
;;                     (goto-char (flymake--diag-beg diag))
;;                     (flycheck-error-new-at (line-number-at-pos)
;;                                            (1+ (- (point) (line-beginning-position)))
;;                                            (pcase (flymake--diag-type diag)
;;                                              ('eglot-error 'error)
;;                                              ('eglot-warning 'warning)
;;                                              ('eglot-note 'info)
;;                                              (_ (error "Unknown diag type, %S" diag)))
;;                                            (flymake--diag-text diag)
;;                                            :checker 'eglot)))
;;                 diags))
;;   (flycheck-buffer))

;; (defun flycheck-eglot--start (checker callback)
;;   (funcall callback 'finished flycheck-eglot-current-errors))

;; (defun flycheck-eglot--available-p ()
;;   (bound-and-true-p eglot--managed-mode))

;; (flycheck-define-generic-checker 'eglot
;;   "Report `eglot' diagnostics using `flycheck'."
;;   :start #'flycheck-eglot--start
;;   :predicate #'flycheck-eglot--available-p
;;   :modes '(prog-mode text-mode))

;; (push 'eglot flycheck-checkers)

;; (defun my-eglot-prefer-flycheck ()
;;   (when eglot--managed-mode
;;     (flycheck-add-mode 'eglot major-mode)
;;     (flycheck-select-checker 'eglot)
;;     (flycheck-mode)
;;     (flymake-mode -1)
;;     (setq eglot--current-flymake-report-fn 'flycheck-eglot-report-fn)))

;; (add-hook 'eglot--managed-mode-hook 'my-eglot-prefer-flycheck)

(provide 'custom-error-check)
;;; custom-error-check.el ends here
