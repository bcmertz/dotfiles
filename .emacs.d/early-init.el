;;; early-init.el --- Early init config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(setq package-enable-at-startup nil) ;; disable package.el

(setq gc-cons-threshold most-positive-fixnum) ;; don't gc on startup

;; security settings
;; source - https://www.jamescherti.com/emacs-security-settings/
(setq gnutls-verify-error t)
(setq tls-checktrust t)
(setq gnutls-min-prime-bits 3072)
(setq enable-local-variables nil)
(setq enable-local-eval nil)
(setq enable-remote-dir-locals nil)  ; Prevent loading dir-locals over TRAMP.

;;; early-init.el ends here
