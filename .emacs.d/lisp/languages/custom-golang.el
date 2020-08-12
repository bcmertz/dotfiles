;;; custom-golang.el --- golang configuration
;;;
;;; Commentary:
;;;
;;; snippets, formatting, docs, autocompletion, etc
;;;
;;; Code:
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)
(require 'company-lsp)
(push 'company-lsp company-backends)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")   ; enable go snippets

;; ;; Set up before-save hooks to format buffer and add/delete imports.
;; ;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :general
  (tyrant-def go-mode-map
    "mr"  'go-rename
    "mp"  'godoc-at-point
    "md" 'godef-describe
    "mt"  'gocode-toggle))

;; (defun go-mode-setup () ; use C-c C-j to jump to definition and C-u C-x C-x to jump back add imports as needed
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   (require 'go-eldoc)
;;   (go-eldoc-setup)                                                ; provides type info
;;   (set-face-attribute 'eldoc-highlight-function-argument nil
;; 		      :foreground "green"
;; 		      :weight 'bold)
;;   (require 'yasnippet)
;;   (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-*")
;;   (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")   ; enable snippets
;;   (yas-global-mode 1)
;;   ;; go mode autocomplete
;;   (require 'company-go)
;;   (customize-set-variable 'company-go-gocode-command "gocode-mod") ; defualt to module support
;;   (set (make-local-variable 'company-backends) '(company-go))
;;   ;; go mode keybindings
;;   (local-set-key (kbd "C-c C-r") 'go-rename)
;;   (local-set-key (kbd "C-c C-p") 'godoc-at-point)
;;   (local-set-key (kbd "C-c C-d") 'godef-describe)
;;   (local-set-key (kbd "C-c C-t") 'gocode-toggle)
;;   )


;; (defun gocode-toggle ()
;;   "Toggle the gocode executable between the module and non-module versions."
;;   (interactive)
;;   (customize-set-variable
;;    'company-go-gocode-command
;;    (if (string= company-go-gocode-command "gocode-mod")
;;        "gocode" "gocode-mod"))
;;   ;; The gocode fork that works with modules is slow, so disable idle completion.
;;   ;; (if (string= company-go-gocode-command "gocode-mod")
;;   ;;     (customize-set-variable 'company-idle-delay nil)
;;   ;;   (custom-reevaluate-setting 'company-idle-delay))
;;   (message company-go-gocode-command))

;; ;;; custom-golang.el ends here
