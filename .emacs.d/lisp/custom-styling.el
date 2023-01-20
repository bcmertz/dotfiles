;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; modeline, scrolling, theme, etc
;;;
;;; Code:

;; cursor behavior
(setq cursor-type 'box)
(setq visible-cursor t)
(blink-cursor-mode 1)

(use-package doom-themes)
(use-package ef-themes)

;; load local themes
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/themes/"))

;; stop asking if my themes are trusted
(setq custom-safe-themes t)

;; default theme; DONT CHANGE to get pywal effects, it is modified in .config/global.cfg
(setq custom-theme 'doom-dracula)    ;; atom-one-dark doom-one ef-cherie doom-gruvbox ewal-doom-one
(setq default-theme custom-theme) ;; default theme to be used by refresh-theme in custom-funcs.el

;; check if we're using pywal to set emacs colorscheme
(if (string= (shell-command-to-string "global_cfg pywal_emacs") "true")
    (progn
      (use-package ewal)
      (setq custom-theme 'ewal-doom-one)))


;; default gui theme
(apply-if-gui 'load-theme custom-theme t)

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;; change theme utility
(defun my-change-theme ()
  "Choose theme from installed list using completing-read."
  (interactive)
  (let ((theme (completing-read "Choose theme : " (custom-available-themes)
                                nil nil nil nil (symbol-name (car custom-enabled-themes)))))
    (disable-all-themes)
    (load-theme (intern theme) t)))

;; TODO add dynamic themeing
(defun my-consult-change-theme ()
  "Choose theme from installed list using consult--read."
  (interactive)
  (let* ((candidates (mapcar #'symbol-name (custom-available-themes)))
         (theme (consult--read
                 candidates
                 :prompt "dynamic theming <C-M-m>: "
                 :default (symbol-name (car custom-enabled-themes))
                 :preview-key (kbd "C-M-m")
                 )))
    (disable-all-themes)
    (load-theme (intern theme) t)
    ))

(defun my-ivy-change-theme ()
  "Choose theme from installed list using ivy-read."
  (interactive)
  (ivy-read "dynamic theming <C-M-m>: " (custom-available-themes)
            :preselect (symbol-name (car custom-enabled-themes))
            :action (lambda (theme)
                      ;; disable enabled themes
                      (disable-all-themes)
                      ;; load new theme
                      (load-theme (intern theme) t))))



;; which key prefix for styling related keybindings
(which-key-add-key-based-replacements "C-c t" "theming")
(global-set-key (kbd "C-c t t") 'my-ivy-change-theme)

;; normally there is no load theme hook, create one here
;; https://www.reddit.com/r/emacs/comments/4v7tcj/does_emacs_have_a_hook_for_when_the_theme_changes/d5wyu1r/
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; true transparency
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql (cond ((numberp alpha-background) alpha-background)
                    ((numberp (cdr alpha-background)) (cdr alpha-background))
                    )
              100)
         '96 '100))))

(global-set-key (kbd "C-c t r") 'toggle-transparency)

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; remove fringes
;; (set-window-fringes neo-global--window 0 0)
;; (set-display-table-slot standard-display-table 0 ?\ )
;; (set-face-attribute 'fringe nil :background nil)
;; (fringe-mode 0)

;; modeline
(use-package doom-modeline
  ;; :load-path "~/coding/doom-modeline"
  :config
  (setq doom-modeline-vcs-max-length 100)
  ;; lsp
  (setq doom-modeline-lsp nil)
  ;; :custom-face
  ;; (mode-line ((t (:foreground "#D8DEE8" :background "#353645"))))
  ;; (mode-line-inactive ((t (:background "#181A1F"))))
  ;; (doom-modeline-buffer-modified ((t (:inherit (error bold) :foreground "#599DD5"))))
  :init (doom-modeline-mode 1))

(use-package hide-mode-line :defer t)

;; center 1 buffer
(use-package centered-window
  :config
  (setq cwm-centered-window-width 130)
  (centered-window-mode t))

;; remove ugly change in bg color in fringes
;; (set-face-attribute 'fringe nil :background nil)

(defun hide-ui-elements ()
  "Hide Scroll bar,menu bar, tool bar."
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(apply-gui-and-term 'hide-ui-elements)

;; toggle tool bar mode
(defun toggle-scroll-bar ()
  "Toggle scroll-bar-mode."
  (interactive)
  (if (eq scroll-bar-mode nil)
      (scroll-bar-mode 1)
    (scroll-bar-mode -1))
  )
(global-set-key (kbd "C-c t s") 'toggle-scroll-bar)

;; toggle tool bar mode
(defun toggle-tool-bar ()
  "Toggle tool-bar-mode."
  (interactive)
  (if (eq tool-bar-mode t)
      (tool-bar-mode -1)
    (tool-bar-mode 1))
  )
(global-set-key (kbd "C-c t l") 'toggle-tool-bar)

;; toggle menu bar mode
(defun toggle-menu-bar ()
  "Toggle menu-bar-mode."
  (interactive)
  (if (eq menu-bar-mode t)
      (menu-bar-mode -1)
    (menu-bar-mode 1))
  )
(global-set-key (kbd "C-c t m") 'toggle-menu-bar)

;; Default font (cant be font with hyphen in the name like current default monospace Inconsolata-g)
(setq initial-frame-alist '((font . "Monospace")))
(setq default-frame-alist '((font . "Monospace")))

;; (add-to-list 'default-frame-alist '(font-backend . "ftcrhb")) ; default i think... https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html

;; cool fonts to try - ETBembo, Source Sans Pro, Lucida Grande, Verdana
;; Emoji: 😄, 🤦, 🏴, ,  ;; should render as 3 color emojis and 2 glyphs
(defun styling/set-fonts()
  "Set the emoji and glyph fonts."
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'prepend)
  (set-face-attribute 'default nil :font "Monospace" :weight 'light :height 110)
  (set-face-attribute 'fixed-pitch nil :font "Monospace" :weight 'light :height 110)
  (set-face-attribute 'variable-pitch nil :font "ETBembo" :weight 'thin :height 118)
  )

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(apply-if-gui 'styling/set-fonts)

;; switch font
(global-set-key (kbd "C-c t f") 'set-frame-font)

(use-package all-the-icons
  :defer t
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


;; describe char face at mouse click
;; from https://emacs.stackexchange.com/a/35449
;; based on: https://emacs.stackexchange.com/a/19585/13444
(defun my-describe-char-at-mouse-click (click-event)
  "`describe-char' at CLICK-EVENT's position, CLICK-EVENT should be a mouse-click event."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((pos (cadr (event-start click-event))))
    (describe-char pos)))

(global-set-key (kbd "C-h <down-mouse-1>")
                #'my-describe-char-at-mouse-click)


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
