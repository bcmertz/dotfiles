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

;; Smooth Scrolling
(require 'sublimity-scroll)
(setq sublimity-scroll-weight 5
      sublimity-scroll-drift-length 10)

;; keyboard scroll one line at a time
(setq scroll-conservatively 101)
(setq scroll-step 1)

;; Highlight current line in gui emacs
(if (display-graphic-p)
    (global-hl-line-mode 1))


(defun styling/turn-on-hl-line ()
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  (interactive)
  (global-hl-line-mode -1))
