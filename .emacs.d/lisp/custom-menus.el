;;; custom-menus.el --- configuration handling right click menus
;;;
;;; Commentary:
;;;
;;; Code:

(eval-after-load "artist"
   '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)
   )

;; example easy-menu
;;
;; (easy-menu-define my-menu my-mode-map "My own menu"
;;   '("My Stuff"
;;     ["One entry" my-function t]
;;     ("Sub Menu"
;;      ["My subentry" my-obscure-function t])))


(provide 'custom-menus)
;;; custom-menus.el ends here
