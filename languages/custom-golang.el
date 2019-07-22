;;;;;;;;;;;;;;;;;;;;;;;;; GOLANG CONFIGURATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(add-to-list 'load-path "~/go/src/github.com/dominikh//") only if not from melpa
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(require 'go-eldoc)
;; autocompletion with company mode
(require 'company)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .5)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(require 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-*")

;; TODO figure out ac-update-greedy (?) error, swap for company mode?
;;(require 'go-autocomplete)
;;(require 'auto-complete-config)
;;(ac-config-default)

(defun go-mode-setup () ; use C-c C-j to jump to definition and C-u C-x C-x to jump back
 (setq gofmt-command "goimports")                                ; format on save, remove/add imports as needed
 (add-hook 'before-save-hook 'gofmt-before-save)
 (go-eldoc-setup)                                                ; provides type info
 (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")   ; enable snippets
 (yas-global-mode 1)

 (require 'company-go)
 (set (make-local-variable 'company-backends) '(company-go))
 (company-mode)

 (local-set-key (kbd "C-c C-r") 'go-rename))                     ; provide go-rename conveniently

(add-hook 'go-mode-hook 'go-mode-setup)
