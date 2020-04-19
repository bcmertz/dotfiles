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
     (setq company-idle-delay .3)  			   ; decrease delay before autocompletion popup shows
     (setq company-echo-delay 0)                           ; remove annoying blinking
     (setq company-begin-commands '(self-insert-command))  ; start autocompletion after typing, if we want to ignore our special tab key we bind below
     ))

(global-company-mode)

;; (global-set-key [M-tab] 'company-complete-common) ;; M-tab triggers autocomplete
(global-set-key [tab] 'tab-indent-or-complete)

;; https://www.emacswiki.org/emacs/CompanyMode#toc11
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
        (if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))
;;(ac-config-default)

;;; custom-autocomplete.el ends here
