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

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-email-notification-types '(count))
  (mu4e-alert-enable-mode-line-display)
  (setq mu4e-change-filenames-when-moving t)
  ;; update script every two minutes
  (setq mu4e-update-interval 120
        mu4e-headers-auto-update t)
        ;; mu4e-get-mail-command "~/.local/bin/mbsync/sort_and_fetch"
  )

;;; custom-email.el ends here
