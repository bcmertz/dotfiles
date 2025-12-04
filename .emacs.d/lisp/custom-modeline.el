;;; custom-modeline.el --- custom modeline -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package doom-modeline
  :config
  (setq doom-modeline-vcs-max-length 100)
  ;; lsp
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-lsp t)
  ;; :custom-face
  ;; (doom-modeline-bar-inactive ((t (:background unspecified))))
  ;; (mode-line ((t (:foreground "#D8DEE8" :background "#353645"))))
  ;; (doom-modeline-buffer-modified ((t (:inherit (error bold) :foreground "#599DD5"))))
  :init
  (defface doom-modeline-bar-inactive
    '((t (:background unspecified)))
    "Face used for the inactive bar."
    :group 'doom-modeline-faces)
  (doom-modeline-mode 1)
  )

(use-package hide-mode-line
  :defer t
  :bind (("C-c t M" . global-hide-mode-line-mode)))

(provide 'custom-modeline)
;;; custom-modeline.el ends here
