;;; custom-tramp.el --- custom tramp config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package tramp
  :straight (:type built-in)
  :defer 0.1
  :init
  ;; https://sqrtminusone.xyz/configs/emacs/#variables-and-environment
  ;; speedup tramp
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
	        vc-ignore-dir-regexp
	        tramp-file-name-regexp))
  (setq tramp-verbose 1) ;; 6 for debugging
  )

(provide 'custom-tramp)
;;; custom-tramp.el ends here
