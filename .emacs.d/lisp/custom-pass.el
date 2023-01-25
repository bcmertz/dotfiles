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

;; ;; pinentry for emacs
;; (use-package pinentry
;;   :config
;;   (setq epa-pinentry-mode 'loopback)
;;   :init
;;   (pinentry-start)
;; )

(provide 'custom-pass)
;;; custom-pass.el ends here
