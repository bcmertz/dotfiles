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

(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Smooth Scrolling
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101
;;       scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01
;;       scroll-preserve-screen-position t
;;       auto-window-vscroll nil
;;       hscroll-margin 5
;;       hscroll-step 5)
;; ;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; (require 'smooth-scrolling)
(require 'sublimity-scroll)
(setq sublimity-scroll-weight 5
      sublimity-scroll-drift-length 10)

;; Highlight current line in gui emacs
(if (display-graphic-p)
    (global-hl-line-mode 1))


(defun styling/turn-on-hl-line ()
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  (interactive)
  (global-hl-line-mode -1))
