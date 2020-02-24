; Style Emacs

;; Themeing
(if (display-graphic-p)
     (add-hook 'after-init-hook (lambda () (load-theme 'atom-one-dark t))))

;; line numbers
(global-display-line-numbers-mode -1)   ; give display numbers

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Smooth Scrolling
(setq scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 5)

;; Turn on and off highlighting of current line
(global-hl-line-mode 1)

(defun styling/turn-on-hl-line ()
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  (interactive)
  (global-hl-line-mode -1))
