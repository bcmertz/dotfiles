;; Packages and repository management
(package-initialize)
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
(add-to-list 'load-path "~/.emacs.d")   ; just in case
(load "general.el")                     ; my configuration
(load "custom.el")                      ; auto-generated config

;;(require 'custom-exwm-config)           ; use exwm instead of default xinitrc
;;(custom-exwm-config)

;; Language specific configuration
(add-to-list 'load-path "~/.emacs.d/languages")
(load "custom-golang.el")
(load "custom-js.el")
