;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; modeline, scrolling, theme, etc
;;;
;;; Code:
;; Themeing
(if (display-graphic-p)
    (add-hook 'after-init-hook (lambda ()
                                 (load-theme 'atom-one-dark t)
                                 ;; (load-theme 'kaolin-temple t)
                                 )))

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; modeline
;; (spaceline-spacemacs-theme)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; center 1 buffer
(require 'sublimity)
(require 'sublimity-attractive)
(sublimity-mode 1)
(setq sublimity-attractive-centering-width 130)

;; Hide Scroll bar,menu bar, tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; Highlight current line in gui emacs
(if (display-graphic-p)
    (global-hl-line-mode 1))

(defun styling/turn-on-hl-line ()
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  (interactive)
  (global-hl-line-mode -1))

;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
                                    (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 1))))

;;; custom-styling.el ends here
