;;; custom-golang.el --- golang configuration
;;;
;;; Commentary:
;;;
;;; snippets, formatting, docs, autocompletion, etc
;;;
;;; Code:
(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :init
  (progn
    (add-hook 'go-mode-hook 'go-mode-setup))
  :general
  (tyrant-def go-mode-map
    "mr"  'go-rename
    "mt"  'gocode-toggle))

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
  (local-set-key (kbd "C-c C-r") 'go-rename))                     ; provide go-rename conveniently

  (require 'company-go)
  (customize-set-variable 'company-go-gocode-command "gocode-mod") ; defualt to module support
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)

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

;;; custom-golang.el ends here
