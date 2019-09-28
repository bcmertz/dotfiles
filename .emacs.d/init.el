;; Don't gc on startup
(setq gc-cons-threshold most-positive-fixnum)

;; Packages and repository management
(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-1" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa-2" . "https://melpa.org/packages/") t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load up any standalone .el files from github or whatever
(add-to-list 'load-path "~/.emacs.d/dependencies")

;; Configuration of emacs in all modes
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "general.el")                     ; my configuration
(load "custom.el")                      ; auto-generated config
(load "styling.el")                     ; styling config

;;(require 'custom-exwm-config)           ; use exwm instead of default xinitrc
;;(custom-exwm-config)

;; Language specific configuration
(add-to-list 'load-path "~/.emacs.d/languages")
(load "custom-golang.el")
(load "custom-js.el")
(load "custom-markdown.el")

;; ;; Sane gc values inside minibufers
;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 800000))

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Return to normal gc value
(setq gc-cons-threshold 800000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes nil)
 '(display-line-numbers nil)
 '(file-name-shadow-mode nil)
 '(gofmt-command "goimports" t)
 '(inhibit-startup-screen t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (flymd exwm ace-window try use-package which-key counsel swiper company-go company flymake-go popup-complete evil neotree direx peep-dired ranger avy nlinum-relative nlinum ag xref-js2 js2-refactor js2-mode go-rename magit markdown-mode go-eldoc go-autocomplete auto-complete flycheck go-mode)))
 '(speedbar-show-unknown-files t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
