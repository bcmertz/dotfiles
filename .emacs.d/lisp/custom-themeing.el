;;; custom-themeing.el --- custom themeing -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

(use-package doom-themes)
(use-package ef-themes)

;; load local themes
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "lisp/themes/"))

;; stop asking if my themes are trusted
(setq custom-safe-themes t)

;; default theme
;; for pywal effects modify ~/.config/global.cfg
(defvar custom-theme 'doom-dracula)     ;; ef-summer atom-one-dark doom-one doom-dracula ef-cherie doom-gruvbox ewal-doom-one
(defvar default-theme custom-theme) ;; default theme to be used by refresh-theme in custom-funcs.el
;; check if we're using pywal to set emacs colorscheme
(if (string= (shell-command-to-string "global_cfg pywal_emacs") "true")
    (progn
      (use-package ewal)
      (setq custom-theme 'ewal-doom-one)))

(defun disable-all-themes ()
  "Disable all themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun update-theme (theme)
  "Update THEME."
  (disable-all-themes)
  (if theme (setq custom-theme (intern theme)))
  (load-theme custom-theme t))

;; default gui theme
(if-gui (update-theme nil))

;; change theme utility
(defun my-change-theme ()
  "Choose theme from installed list using `completing-read'."
  (interactive)
  (let ((theme (completing-read "Choose theme : " (custom-available-themes)
                                nil nil nil nil (symbol-name (car custom-enabled-themes)))))
    (update-theme theme)))

(defun my-consult-change-theme ()
  "Choose theme from installed list using `consult--read'."
  (interactive)
  (consult--read
   (mapcar #'symbol-name (custom-available-themes))
   :require-match t
   :prompt "Choose theme (dynamic C-<return>): "
   :default (symbol-name (car custom-enabled-themes))
   :state (lambda (action theme)
            (pcase action
              ('return (if theme (update-theme theme)))
              ('preview (if theme (update-theme theme)))))))

(defun my-ivy-change-theme ()
  "Choose theme from installed list using `ivy-read'."
  (interactive)
  (ivy-read "Choose theme (dynamic <C-M-m>): " (custom-available-themes)
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

(provide 'custom-themeing)
;;; custom-themeing.el ends here
