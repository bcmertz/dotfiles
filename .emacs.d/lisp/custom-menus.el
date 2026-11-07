;;; custom-menus.el --- configuration handling right click menus -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package util-funcs
  :straight (util-funcs :type nil :local-repo "~/.emacs.d/lisp/lib/util-funcs/"))

;; make right-click behave normally with context menu
(if-gui (context-menu-mode))

(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation))

;; example easy-menu
;;
;; (easy-menu-define my-menu my-mode-map "My own menu"
;;   '("My Stuff"
;;     ["One entry" my-function t]
;;     ("Sub Menu"
;;      ["My subentry" my-obscure-function t])))


(provide 'custom-menus)
;;; custom-menus.el ends here
