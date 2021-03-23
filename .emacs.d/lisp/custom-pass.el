;;; custom-pass.el --- pass configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package pass
  :defer t
  :ensure t
  :config
  (setq pass-username-fallback-on-filename t)
  )

;;; custom-pass.el ends here
