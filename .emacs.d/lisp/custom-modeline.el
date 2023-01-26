;;; custom-modeline.el --- custom modeline
;;;
;;; Commentary:
;;;
;;; Code:

(use-package doom-modeline
  :config
  (setq doom-modeline-vcs-max-length 100)
  ;; lsp
  (setq doom-modeline-lsp nil)
  ;; :custom-face
  ;; (mode-line ((t (:foreground "#D8DEE8" :background "#353645"))))
  ;; (mode-line-inactive ((t (:background "#181A1F"))))
  ;; (doom-modeline-buffer-modified ((t (:inherit (error bold) :foreground "#599DD5"))))
  :init (doom-modeline-mode 1))

(use-package hide-mode-line
  :defer t
  :bind (("C-c t M" . global-hide-mode-line-mode)))

(provide 'custom-modeline)
;;; custom-modeline.el ends here
