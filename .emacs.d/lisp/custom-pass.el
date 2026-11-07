;;; custom-pass.el --- pass configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package pass
  :defer t
  :config
  (setq pass-username-fallback-on-filename t))

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;; use auth-source-pass as the primary auth-source provider
;; so that all passwords are stored in a single place.
(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; By default, Emacs stores sensitive authinfo credentials as unencrypted text
;; in your home directory. Use GPG to encrypt the authinfo file for enhanced
;; security.
(setq auth-sources (list "~/.authinfo.gpg"))

(provide 'custom-pass)
;;; custom-pass.el ends here
