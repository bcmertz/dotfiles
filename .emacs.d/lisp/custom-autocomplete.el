;;; custom-autocomplete.el --- autocomplete configuration
;;;
;;; Commentary:
;;;
;;; autocompletion with company mode
;;;
;;; Code:
(use-package company
   :defer t
   :init
   (progn
     (setq company-tooltip-limit 20)                       ; bigger popup window
     ;; (setq company-idle-delay nil)                         ; don't autocomplete on typing, backtab instead
     (setq company-idle-delay .3)  			      ; decrease delay before autocompletion popup shows
     ;;(setq company-echo-delay 0)                           ; remove annoying blinking
     ;;(setq company-begin-commands '(self-insert-command))  ; start autocompletion after typing, if we want to ignore our special tab key we bind below
     ))

(global-company-mode)

(require 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-*")
(yas-global-mode 1)

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

;; (global-set-key [tab] 'tab-indent-or-complete)

(global-set-key [backtab] 'company-complete-common) ;; backtab triggers autocomplete

;;; custom-autocomplete.el ends here
