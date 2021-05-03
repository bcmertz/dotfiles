;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; modeline, scrolling, theme, etc
;;;
;;; Code:

;; default gui theme
(setq custom-theme 'doom-one) ;; atom-one-dark
(apply-if-gui 'load-theme custom-theme t)

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; transparency (focused . unfocused)
;; (set-frame-parameter (selected-frame) 'alpha '(100 . 85))
;; (add-to-list 'default-frame-alist '(alpha . (100 . 85)))

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
(use-package centered-window
  :ensure t
  :config
  (setq cwm-centered-window-width 130)
  (centered-window-mode t))


;; Hide Scroll bar,menu bar, tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; TODO find a good monospace font
;; Default font (cant be font with hyphen in the name like current default monospace Inconsolata-g)
;; (setq initial-frame-alist '((font . "Monospace")))
;; (setq default-frame-alist '((font . "Monospace")))

;; Emoji: 😄, 🤦, 🏴, ,  ;; should render as 3 color emojis and 2 glyphs
(defun styling/set-backup-fonts()
  "Set the emoji and glyph fonts."
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'append)
  )

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(apply-if-gui 'styling/set-backup-fonts)

(use-package all-the-icons
  :defer t
  :ensure t
  )

;; Highlight current line in gui emacs
(apply-if-gui 'global-hl-line-mode 1)

(defun styling/turn-on-hl-line ()
  "Turn on global hl line mode."
  (interactive)
  (global-hl-line-mode 1))

(defun styling/turn-off-hl-line ()
  "Turn off global hl line mode."
  (interactive)
  (global-hl-line-mode -1))


;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))


;; smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; nil disables automatic horizontal scrolling
(setq auto-hscroll-mode t
      ;; how close to the edge of the window before the window is scrolled
      hscroll-margin 2
      ;; how many columns to scroll the window
      ;; 0 horiz scrolling centers point horizontally within the window
      ;; positive integer: # of cols to scroll by
      ;; floating point: fraction of windows width to scroll by
      hscroll-step 1
      )


(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
                                    (if truncate-lines (scroll-right 5))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 5))))
(global-set-key (kbd "<S-mouse-5>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 10))))
(global-set-key (kbd "<S-mouse-4>") (lambda () (interactive)
                                        (if truncate-lines (scroll-right 10))))

;;; custom-styling.el ends here
