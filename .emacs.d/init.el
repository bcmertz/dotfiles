;; Don't gc on startup
(setq gc-cons-threshold most-positive-fixnum)

;;(require 'custom-exwm-config)           ; use exwm instead of default xinitrc
;;(custom-exwm-config)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))
(load "custom-general.el")
(load "custom-funcs.el")
(load "custom-styling.el")
(load "custom-navigation.el")
(load "custom-editing.el")
(load "custom-windowing.el")
(load "custom-golang.el")
(load "custom-js.el")
(load "custom-markdown.el")
(load "custom-css.el")

;; set customize config file location
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Return to normal gc value
(setq gc-cons-threshold 800000)
;; garbage collect when emacs leaves focus
(add-hook 'focus-out-hook 'garbage-collect)
