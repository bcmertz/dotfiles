;;; custom-pass.el --- pass configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; emacs GnuPG library
(use-package epg
  :straight (:type built-in)
  :config
  (setq epg-gpg-program "gpg2")
  (setq epg-pinentry-mode 'loopback))

;; user interface to GnuPG, part of EPG
(use-package epa
  :straight (:type built-in)
  :config
  (setq epa-file-select-keys t)
  (setq epa-file-encrypt-to nil))

(use-package pinentry
  :config
  (pinentry-start))

;; By default, Emacs stores sensitive authinfo credentials as unencrypted text
;; in your home directory. Use GPG to encrypt the authinfo file for enhanced
;; security.
(setq auth-sources (list "~/.authinfo.gpg"))
(setq auth-source-cache-expiry 3600)

(defun my-security-clear-caches ()
  "Clear all cached authentication data managed by auth-source."
  (when (fboundp 'auth-source-forget-all-cached)
    (auth-source-forget-all-cached)))

;; clear cached authentication data after 15 minutes of inactivity
(run-with-idle-timer 900 t #'my-security-clear-caches)

;; use auth-source-pass as the primary auth-source provider
;; so that all passwords are stored in a single place.
(use-package auth-source-pass
  :straight (:type built-in)
  :config
  (auth-source-pass-enable))

(use-package pass
  :defer t
  :config
  (setq pass-username-fallback-on-filename t))

(provide 'custom-pass)
;;; custom-pass.el ends here
