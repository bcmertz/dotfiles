;;; custom-email.el --- custom email
;;;
;;; Commentary:
;;;
;;; email support
;;; configured with https://github.com/cemkeylan/mu-wizard/
;;; rest of config is in ~/.config/mu4e and ~/.config/msmtp
;;; mu-wizard handles ~/.config config
;;;
;;; Code:

(use-package mu4e
  :load-path "~/.emacs.d/elpa/mu4e" ;; requires moving /usr/share/emacs/site-lisp/mu4e
  )

(use-package mu4e-config
  :after mu4e
  :load-path "~/.config/mu4e")

;;; custom-email.el ends here
