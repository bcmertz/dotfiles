;;;;;;;;;;;;;;;;;;;;;;;;; GOLANG CONFIGURATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/go/src/github.com/dominikh//") only if not from melpa
;; (autoload 'go-mode "go-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :init
  (progn
    (add-hook 'go-mode-hook 'go-mode-setup)))

(defun go-mode-setup () ; use C-c C-j to jump to definition and C-u C-x C-x to jump back
 (setq gofmt-command "goimports")                                ; format on save, remove/add imports as needed
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
 (set (make-local-variable 'company-backends) '(company-go))
 (company-mode)

 (local-set-key (kbd "C-c C-r") 'go-rename))                     ; provide go-rename conveniently

;; figure out ac-update-greedy (?) error, swap for company mode?
;;(load ""go-autocomplete.el"")
;;(require 'auto-complete-config)
;;(ac-config-default)
