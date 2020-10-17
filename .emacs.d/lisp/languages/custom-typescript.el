;;; custom-typescript.el --- js configuration
;;;
;;; Commentary:
;;;
;;; Code:

;; (use-package typescript-mode
;;   :defer t
;;   :init
;;   (setq typescript-indent-level 2)
;;   )

(use-package web-mode
  :defer t
  :mode (
         ("\\.[jt]sx\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
         )
  :after flycheck
  )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    ;;(flycheck-select-checker 'typescript-tslint)
    ;;(flycheck-add-mode 'typescript-tslint 'web-mode)
    )

(add-hook 'web-mode-hook  'my-web-mode-hook)

;;; custom-typescript.el ends here
