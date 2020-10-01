;;; init.el --- loads various configuration layers
;;;
;;; Commentary:
;;;
;;; This is my personal config, based heavily on clarete, spacemacs, zamanksy, sam217pa
;;;
;;; Code:
(setq gc-cons-threshold most-positive-fixnum) ; Don't gc on startup

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))

;; emacs as window manager
(setq session (getenv "SESSION"))
(if (equal session "emacs")
    (progn
      (message "managing windows")
      (require 'custom-exwm-config)
      (custom-exwm-config)
      )
  )

;; general configuration
(load "custom-general.el")
(load "custom-evil-keymap") ;; toggle evil with M-SPC; SPC is leader key
(load "custom-funcs.el")
(load "custom-styling.el")
(load "custom-autocomplete.el")
(load "custom-navigation.el")
(load "custom-editing.el")
(load "custom-windowing.el")
(load "custom-flycheck.el")
(load "custom-lsp.el")
(load "custom-git.el")
(load "custom-compile.el")

;; mode configurations
(load "custom-golang.el")
(load "custom-js.el")
(load "custom-typescript.el")
(load "custom-markdown.el")
(load "custom-css.el")
(load "custom-html.el")
(load "custom-org.el")
(load "custom-latex.el")

;; set customize config file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Return to normal gc value
(setq gc-cons-threshold 800000)
;; garbage collect when emacs leaves focus
(add-hook 'focus-out-hook 'garbage-collect)

;;; init.el ends here
