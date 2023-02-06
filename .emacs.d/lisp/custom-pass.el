;;; custom-pass.el --- pass configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package pass
  :defer t
  :config
  (setq pass-username-fallback-on-filename t)
  )

;; use auth-source-pass as the primary auth-source provider
;; so that all passwords are stored in a single place.
(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; ;; pinentry for emacs
;; (use-package pinentry
;;   :config
;;   (setq epa-pinentry-mode 'loopback)
;;   :init
;;   (pinentry-start)
;; )

(provide 'custom-pass)
;;; custom-pass.el ends here
