;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; modeline, scrolling, theme, etc
;;;
;;; Code:

;; load local themes
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/themes/"))
;; TtLl
;; stop asking if my themes are trusted
(setq custom-safe-themes t)

;; default theme; DONT CHANGE to get pywal effects, it is modified in .config/global.cfg
(setq custom-theme 'doom-dracula)     ;; atom-one-dark doom-one doom-dracula doom-gruvbox ewal-doom-one
(setq default-theme custom-theme) ;; default theme to be used by refresh-theme in custom-funcs.el

;; check if we're using pywal to set emacs colorscheme
(if (string= (shell-command-to-string "global_cfg pywal_emacs") "true")
    (progn
      (use-package ewal)
      (setq custom-theme 'ewal-doom-one)))


;; default gui theme
(apply-if-gui 'load-theme custom-theme t)

;; change theme utility
(defun change-theme ()
  "Choose theme from installed list."
  (interactive)
  (ivy-read "dynamic theming <C-M-m>: " (custom-available-themes)
            :preselect (symbol-name (car custom-enabled-themes))
            :action (lambda (theme)
                      ;; disable enabled themes
                      (dolist (i custom-enabled-themes)
                        (disable-theme i))
                      ;; load new theme
                      (load-theme (intern theme) t))))

;; which key prefix for styling related keybindings
(which-key-add-key-based-replacements "C-c t" "theming")
(global-set-key (kbd "C-c t t") 'change-theme)


;; transparency (focused . unfocused)
;; (setq default-transparency (list 100 100))
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(97 . 92) '(100 . 100)))))

(global-set-key (kbd "C-c t r") 'toggle-transparency)

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 2 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; modeline
(use-package doom-modeline
  ;; :load-path "~/coding/doom-modeline"
  :ensure t
  :config
  (setq doom-modeline-vcs-max-length 100)
  (setq doom-modeline-persp-icon t)
  :custom-face
  (mode-line ((t (:foreground "#D8DEE8" :background "#353645"))))
  (mode-line-inactive ((t (:background "#181A1F"))))
  (doom-modeline-buffer-modified ((t (:inherit (error bold) :foreground "#599DD5"))))
  :init (doom-modeline-mode 1))

(use-package hide-mode-line
  :ensure t)

;; center 1 buffer
(use-package centered-window
  :ensure t
  :config
  (setq cwm-centered-window-width 130)
  (centered-window-mode t))

;; remove ugly change in bg color in fringes
(set-face-attribute 'fringe nil :background nil)

;; Hide Scroll bar,menu bar, tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; toggle tool bar mode
(defun toggle-tool-bar ()
  "Toggle tool-bar-mode."
  (interactive)
  (if (eq tool-bar-mode t)
      (tool-bar-mode -1)
    (tool-bar-mode 1))
  )

;; toggle tool
(global-set-key (kbd "C-c t l") 'toggle-tool-bar)


;; toggle menu bar mode
(defun toggle-menu-bar ()
  "Toggle menu-bar-mode."
  (interactive)
  (if (eq menu-bar-mode t)
      (menu-bar-mode -1)
    (menu-bar-mode 1))
  )

;; toggle menu
(global-set-key (kbd "C-c t m") 'toggle-menu-bar)

;; Default font (cant be font with hyphen in the name like current default monospace Inconsolata-g)
(setq initial-frame-alist '((font . "Monospace")))
(setq default-frame-alist '((font . "Monospace")))




;; Emoji: 😄, 🤦, 🏴, ,  ;; should render as 3 color emojis and 2 glyphs
(defun styling/set-fonts()
  "Set the emoji and glyph fonts."
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'prepend)
  (set-face-attribute 'default nil :font "Monospace" :weight 'light :height 110)
  (set-face-attribute 'fixed-pitch nil :font "Monospace" :weight 'light :height 120)
  (set-face-attribute 'variable-pitch nil :font "ETBembo" :weight 'thin :height 120)
  )

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(apply-if-gui 'styling/set-fonts)

;; switch font
(global-set-key (kbd "C-c t f") 'set-frame-font)

(use-package all-the-icons
  :defer t
  :ensure t
  :config
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))  (all-the-icons-install-fonts t))
  )

;; Highlight current line in gui emacs
(apply-if-gui 'global-hl-line-mode 1)

;; toggle hl line mode globally
(defun toggle-hl-line ()
  "Toggle global hl line mode."
  (interactive)
  (if (eq global-hl-line-mode t)
      (global-hl-line-mode -1)
    (global-hl-line-mode 1))
  )

;; toggle hl line mode
(global-set-key (kbd "C-c t h") 'toggle-hl-line)


;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))


;; smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
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
