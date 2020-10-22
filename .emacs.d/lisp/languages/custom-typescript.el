;;; custom-typescript.el --- ts configuration
;;;
;;; Commentary: This is really just the web mode config but since we use it for tsx files primarily here it is
;;;
;;; Code:

(use-package typescript-mode
  :defer t
  :init
  (setq typescript-indent-level 2)
  )

(use-package web-mode
  :defer t
  :mode (
         ("\\.[jt]sx\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
         )
  )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)
    )

(add-hook 'web-mode-hook  'my-web-mode-hook)

;;; custom-typescript.el ends here
