;;; custom-general.el --- general emacs configuration -*- lexical-binding: t -*-
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

;; confirm we want to exit the garden
;; (setq confirm-kill-emacs 'y-or-n-p)

;; save place in files
(save-place-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; unbind C-z suspend unless were in a terminal where it's useful
(if-gui (global-unset-key (kbd "C-z")))

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
