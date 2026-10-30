;;; custom-native-comp.el --- custom native-comp -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; native-comp configuration
;;;
;;; Code:

(use-package comp-run
  :straight (:type built-in)
  :config
  ;; save battery power
  (setq native-comp-async-on-battery-power nil)
  ;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
  (push "tramp-loaddefs.el.gz" native-comp-jit-compilation-deny-list)
  (push "cl-loaddefs.el.gz" native-comp-jit-compilation-deny-list))

(provide 'custom-native-comp)
;;; custom-native-comp.el ends here
