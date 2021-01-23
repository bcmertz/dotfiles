;;; custom-email.el --- custom email
;;;
;;; Commentary:
;;;
;;; email support
;;;
;;; Code:

(load-file "~/.config/mu4e/mu4e-config.el")

(add-to-list 'load-path "~/.config/mu4e")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'mu4e-config)

(use-package mu4e-config
  :after mu4e
  :load-path "~/.config/mu4e")


;;; custom-email.el ends here
