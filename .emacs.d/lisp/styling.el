; Style Emacs

;; Themeing
(if (display-graphic-p)
    (add-hook 'after-init-hook (lambda ()
                                 (load-theme 'atom-one-dark t)
                                 ;; (load-theme 'kaolin-ocean t)
                                 )))

;; line numbers
(global-display-line-numbers-mode -1)   ; give display numbers

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; modeline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; center 1 buffer
(require 'sublimity)
(require 'sublimity-attractive)
(sublimity-mode 1)
(setq sublimity-attractive-centering-width 130)
(sublimity-attractive-hide-bars)

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
