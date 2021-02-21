;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; modeline, scrolling, theme, etc
;;;
;;; Code:

;; Themeing
(if (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (if (display-graphic-p frame)
        (load-theme 'atom-one-dark t)
      )
    )
  )
)

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; modeline
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-vcs-max-length 100)
  (setq doom-modeline-persp-icon t)
  :custom-face
  (mode-line ((t (:foreground "#D8DEE8" :background "#353645"))))
  (mode-line-inactive ((t (:background "#181A1F"))))
  (doom-modeline-buffer-modified ((t (:inherit (error bold) :foreground "#599DD5"))))
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

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; Highlight current line in gui emacs
(if (display-graphic-p)
    (global-hl-line-mode 1))

(defun styling/turn-on-hl-line ()
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  (interactive)
  (global-hl-line-mode -1))

;; smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

(setq auto-hscroll-mode nil)

(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
                                    (if truncate-lines (scroll-right 5))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 5))))
(global-set-key (kbd "<S-mouse-5>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 10))))
(global-set-key (kbd "<S-mouse-4>") (lambda () (interactive)
                                        (if truncate-lines (scroll-right 10))))

;;; custom-styling.el ends here
