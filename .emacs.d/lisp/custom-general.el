;;; custom-general.el --- general emacs configuration
;;;
;;; Commentary:
;;;
;;; general settings, input/output
;;;
;;; Code:

;; see our todo
;; (setq initial-buffer-choice "~/docs/org/notes.org")

(yas-minor-mode)
;; backup and tmp files
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; confirm we want to exit the garden
;; (setq confirm-kill-emacs 'y-or-n-p)

;; save place in files
(save-place-mode 1)

;; unbind C-z suspend unless were in a terminal where it's useful
(defun unbind-suspend ()
  (global-unset-key (kbd "C-z")))
(apply-if-gui 'unbind-suspend)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; use y/n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Bind keyboard-escape-quit to ESC instead of ESC ESC ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; stop noises
(setq ring-bell-function 'ignore)

;; stop server client instructions
(setq server-client-instructions nil)

;; display-time-world command provides a nice display of the time at a specified list of timezones.
;; Nice for working in a team with remote members.
(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Seattle")
        ("America/New_York" "New York")
        ("Europe/Athens" "Athens")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; terminal specific escape codes
;; found with showkey -a
(add-hook 'tty-setup-hook
          '(lambda ()
             (define-key input-decode-map "\e[1;3A" [M-up])
             (define-key input-decode-map "\e[1;3B" [(meta down)])
             (define-key input-decode-map "\e[1;3C" [(meta right)])
             (define-key input-decode-map "\e[1;3D" [(meta left)])
             (define-key input-decode-map "\e[1;2A" [S-up])
             (define-key input-decode-map "\e[1;2B" [(shift down)])
             (define-key input-decode-map "\e[1;2C" [(shift right)])
             (define-key input-decode-map "\e[1;2D" [(shift left)])
             (define-key input-decode-map "\e[1;5A" [(C-up)])
             (define-key input-decode-map "\e[1;5B" [(control down)])
             (define-key input-decode-map "\e[1;5C" [(control right)])
             (define-key input-decode-map "\e[1;5D" [(control left)])
             (define-key input-decode-map "\e[1;7A" [(C-M-up)])
             (define-key input-decode-map "\e[1;7B" [(C-M-down)])
             (define-key input-decode-map "\e[1;7C" [(C-M-right)])
             (define-key input-decode-map "\e[1;7D" [(C-M-left)])
             ;; quick and dirty way to bind C-/ to undo in terminal
             (global-set-key (kbd "C-_") 'undo)
             ))
(if (equal "st-meta-256color" (tty-type))
    (define-key input-decode-map "\e[1;2A\" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map \"\e[1;2A\" [S-up]))]))]"))

;;; custom-general.el ends here
