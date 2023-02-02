;;; custom-scrolling.el --- custom scrolling
;;;
;;; Commentary:
;;;
;;; Code:

;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; smooth scrolling
(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-interpolate-page nil)

(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; horizontal scrolling
(setq auto-hscroll-mode t
      ;; how close to the edge of the window before the window is scrolled
      hscroll-margin 2
      ;; how many columns to scroll the window
      hscroll-step 1
      )

(mouse-wheel-mode t)

(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t
      mouse-wheel-scroll-amount-horizontal 1
      mouse-wheel-progressive-speed nil
      )

;; (global-set-key (kbd "<mouse-4>") (lambda () (interactive)
;;                                     (if truncate-lines (scroll-right 5))))
;; (global-set-key (kbd "<mouse-5>") (lambda () (interactive)
;;                                     (if truncate-lines (scroll-left 5))))
;; (global-set-key (kbd "<S-mouse-5>") (lambda () (interactive)
;;                                       (if truncate-lines (scroll-left 10))))
;; (global-set-key (kbd "<S-mouse-4>") (lambda () (interactive)
;;                                       (if truncate-lines (scroll-right 10))))

(provide 'custom-scrolling)
;;; custom-scrolling.el ends here
