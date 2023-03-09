;;; custom-general.el --- general emacs configuration
;;;
;;; Commentary:
;;;
;;; general settings, input/output
;;;
;;; Code:

;; handle various warnings from flymake, nil faces, etc
;; (setq warning-suppress-log-types '((set-face-attribute)))
(setq warning-minimum-log-level :error)
(setq warning-suppress-log-types '((Warning: setting attribute)))

;; "Pure" gtk
;; (when (eq window-system 'pgtk)
;;   (pgtk-use-im-context t))

;; backup and tmp files
;; (setq make-backup-files nil)
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq make-backup-files t
      backup-by-copying t
      kept-new-versions 200
      kept-old-versions 0
      delete-old-versions t
      vc-make-backup-files t
      version-control t)
(setq create-lockfiles nil) ;; can't set to a different directory so disable :/


(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook #'force-backup-of-buffer)

;; use this fork for proper diff ordering (logically reversed by default)
(use-package backup-walker
  :straight (backup-walker :type git
                           :host github
                           :repo "snowman/backup-walker")
  :defer t
  :bind (:map backup-walker-mode-map (("R" . diff-reverse-direction)))
  )

(use-package minibuffer
  :straight nil
  :config
  (setq enable-recursive-minibuffers t)    ;; Use the minibuffer whilst in the minibuffer
  )

;; confirm we want to exit the garden
;; (setq confirm-kill-emacs 'y-or-n-p)

;; save place in files
(save-place-mode 1)

;; save history of minibuffer prompts
(savehist-mode 1)
(setq history-length 200)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

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

(use-package openwith
  :straight (openwith :local-repo "~/.emacs.d/lisp/lib/openwith"
                      :files ("openwith.el")
                      :type nil)
  :config
  (openwith-mode)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         ;; (list (openwith-make-extension-regexp
         ;;        '("xbm" "pbm" "pgm" "ppm" "pnm"
         ;;          "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
         ;;       "nsxiv"
         ;;       '(file))
         ;; (list (openwith-make-extension-regexp
         ;;        '("pdf"))
         ;;       "zathura"
         ;;       '(file))
         )))

(provide 'custom-general)
;;; custom-general.el ends here
