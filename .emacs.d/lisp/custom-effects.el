;;; custom-effects.el --- custom effects -*- lexical-binding: t -*-
;;;
;;; Commentary: graphical effects that could be fun
;;; to setup later
;;;
;;; Code:

;; https://github.com/alphapapa/snow.el
;; https://github.com/elizagamedev/power-mode.el
;; https://github.com/emacsmirror/redacted
;; https://www.emacswiki.org/emacs/ZoneMode

(use-package artist
  :defer t
  :straight (:type built-in)
  :bind (:map artist-mode-map
              ("<down-mouse-2>" . artist-mouse-choose-operation)))

(provide 'custom-effects)
;;; custom-effects.el ends here
