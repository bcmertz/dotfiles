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

;; Return to normal gc value
(setq gc-cons-threshold 800000)
;; garbage collect when emacs leaves focus
(add-hook 'focus-out-hook 'garbage-collect)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gofmt-command "goimports" t)
 '(package-selected-packages
   '(general yasnippet which-key vimish-fold use-package sublimity spaceline smex smartparens projectile neotree multiple-cursors markdown-mode magit go-eldoc flycheck expand-region evil counsel company-go cask avy-zap atom-one-dark-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
