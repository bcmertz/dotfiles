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

;; Configuration of emacs in all modes
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "general.el")                     ; my configuration
(load "custom.el")                      ; auto-generated config
(load "styling.el")                     ; styling config
(add-hook 'after-init-hook (lambda () (load-theme 'monokai t)))

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
