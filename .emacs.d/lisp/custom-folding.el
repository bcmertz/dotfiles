;;; custom-folding.el --- custom folding -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; builtin code folding
;; https://www.emacswiki.org/emacs/HideShow
(use-package hs-minor-mode
  :straight (:type built-in)
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (setq hs-minor-mode-map (make-sparse-keymap)) ;; turn off default keymap
  (which-key-add-key-based-replacements "C-c f" "folding") ;; add our own keymap
  :config
  (setq hs-hide-comments nil)
  :bind
  ("C-c f f" . hs-hide-block)
  ("C-c f t" . hs-toggle-hiding)
  ("C-c f d" . hs-show-block)
  ("C-c f a" . hs-show-all)
  ("M-`" . hs-show-all))

(provide 'custom-folding)
;;; custom-folding.el ends here
