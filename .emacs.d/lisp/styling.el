; Style Emacs

;; Themeing
(if (display-graphic-p)
     (add-hook 'after-init-hook (lambda () (load-theme 'monokai t))))

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
(setq styling/hl-line-enabled t)

(defun styling/turn-on-hl-line ()
  (interactive)
  (when eos/hl-line-enabled
    (hl-line-mode 1)))

(defun styling/turn-off-hl-line ()
  (interactive)
  (hl-line-mode -1))

;; (ace-popup-menu-mode 1) ; cant get it to wokr with ac
