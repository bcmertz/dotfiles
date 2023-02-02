;;; custom-styling.el --- custom styling
;;;
;;; Commentary:
;;;
;;; theme, UI, highlighting
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

(defun update-theme (theme)
  "Update theme."
  (disable-all-themes)
  (load-theme (intern theme) t))

;; change theme utility
(defun my-change-theme ()
  "Choose theme from installed list using completing-read."
  (interactive)
  (let ((theme (completing-read "Choose theme : " (custom-available-themes)
                                nil nil nil nil (symbol-name (car custom-enabled-themes)))))
    (update-theme theme)))

(defun my-consult-change-theme ()
  "Choose theme from installed list using consult--read."
  (interactive)
  (consult--read
   (mapcar #'symbol-name (custom-available-themes))
   :require-match t
   :prompt "dynamic theming <M-return>: "
   :default (symbol-name (car custom-enabled-themes))
   :state (lambda (action theme)
            (pcase action
              ('return (if theme (update-theme theme)))
              ('preview (if theme (update-theme theme)))))))

(defun my-ivy-change-theme ()
  "Choose theme from installed list using ivy-read."
  (interactive)
  (ivy-read "dynamic theming <C-M-m>: " (custom-available-themes)
            :preselect (symbol-name (car custom-enabled-themes))
            :action (lambda (theme)
                      ;; disable enabled themes
                      (update-theme theme))))

;; which key prefix for styling related keybindings
(which-key-add-key-based-replacements "C-c t" "theming")
(global-set-key (kbd "C-c t t") 'my-consult-change-theme)

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
;;
;; unable to describe-char in modeline, fringe, etc because describe-char
;; only works in accessible buffers unfortunately. Can get the text though,
;; see https://emacs.stackexchange.com/a/311
(defun my-describe-char-at-mouse-click (click-event)
  "`describe-char' at CLICK-EVENT's position, CLICK-EVENT should be a mouse-click event."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((pos (cadr (event-start click-event))))
    (describe-char pos)))

(global-set-key (kbd "C-h <down-mouse-1>")
                #'my-describe-char-at-mouse-click)


(provide 'custom-styling)
;;; custom-styling.el ends here
