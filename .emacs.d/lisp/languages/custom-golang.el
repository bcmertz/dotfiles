;;;;;;;;;;;;;;;;;;;;;;;;; GOLANG CONFIGURATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/go/src/github.com/dominikh//") only if not from melpa
;; (autoload 'go-mode "go-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :init
  (progn
    (add-hook 'go-mode-hook 'go-mode-setup)))

(defun go-mode-setup () ; use C-c C-j to jump to definition and C-u C-x C-x to jump back add imports as needed
 (add-hook 'before-save-hook 'gofmt-before-save)

 (require 'go-eldoc)
 (go-eldoc-setup)                                                ; provides type info
 (set-face-attribute 'eldoc-highlight-function-argument nil
		     :foreground "green"
		     :weight 'bold)

 (require 'yasnippet)
 (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-*")
 (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")   ; enable snippets
 (yas-global-mode 1)

 ;; autocompletion with company mode
 (use-package company
   :defer t
   :init
   (progn
     (setq company-tooltip-limit 20)                       ; bigger popup window
     (setq company-idle-delay nil)                         ; don't autocomplete on typing, backtab instead
     ;; (setq company-idle-delay .3)  			   ; decrease delay before autocompletion popup shows
     (setq company-echo-delay 0)                           ; remove annoying blinking
     ))
;; (setq company-begin-commands '(self-insert-command))  ; start autocompletion after typing, if we want to ignore our special tab key we bind below

 (require 'company-go)
 (customize-set-variable 'company-go-gocode-command "gocode-mod") ; defualt to module support
 (set (make-local-variable 'company-backends) '(company-go))
 (company-mode)
 (global-set-key [backtab] 'company-complete-common) ;; backtab triggers autocomplete 
 ;; (global-set-key [tab] 'tab-indent-or-complete)
 
 (local-set-key (kbd "C-c C-r") 'go-rename))                     ; provide go-rename conveniently

;; figure out ac-update-greedy (?) error, swap for company mode?
;;(load ""go-autocomplete.el"")
;;(require 'auto-complete-config)
;;(ac-config-default)

(defun gocode-toggle ()
  "Toggle the gocode executable between the module and non-module versions."
  (interactive)
  (customize-set-variable
   'company-go-gocode-command
   (if (string= company-go-gocode-command "gocode-mod")
       "gocode" "gocode-mod"))

  ;; The gocode fork that works with modules is slow, so disable idle completion.
  ;; (if (string= company-go-gocode-command "gocode-mod")
  ;;     (customize-set-variable 'company-idle-delay nil)
  ;;   (custom-reevaluate-setting 'company-idle-delay))
  (message company-go-gocode-command))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

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

(provide 'custom-golang)
