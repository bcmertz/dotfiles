; Style Emacs

;; Themeing
(if (display-graphic-p)
     (add-hook 'after-init-hook (lambda () (load-theme 'monokai t))))

;; Custom monokai theme values
;; (setq ;; foreground and background
;;  monokai-foreground     "#ABB2BF"
;;  monokai-background     "#282C34"
;;  ;; highlights and comments
;;  monokai-comments       "#F8F8F0"
;;  monokai-emphasis       "#282C34"
;;  ;;monokai-highlight      "#FFB269"
;;  monokai-highlight-alt  "#6A6637"
;;  monokai-highlight-line "#1B1D1E"
;;  monokai-line-number    "#F8F8F0"
;;  ;; colours
;;  monokai-blue           "#61AFEF"
;;  monokai-cyan           "#56B6C2"
;;  monokai-green          "#98C379"
;;  monokai-gray           "#3E4451"
;;  monokai-violet         "#C678DD"
;;  monokai-red            "#E06C75"
;;  monokai-orange         "#D19A66"
;;  monokai-yellow         "#E5C07B"
;;  )

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
(setq styling/hl-line-enabled t)

(defun styling/turn-on-hl-line ()
  (interactive)
  (when eos/hl-line-enabled
    (hl-line-mode 1)))

(defun styling/turn-off-hl-line ()
  (interactive)
  (hl-line-mode -1))

;; (ace-popup-menu-mode 1) ; cant get it to wokr with ac
