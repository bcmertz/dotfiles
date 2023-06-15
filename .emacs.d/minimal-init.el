;;; minimal-init.el --- loads minimal configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; used for debugging when i break the regular config
;;;
;;; Code:

;; load paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set customize config file location and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

;;;;;;;;;;;;;;;;;;; straight packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rebuild changed packages on startup, slower
(setq straight-check-for-modifications '(find-at-startup find-when-checking))
;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package
(straight-use-package 'use-package)
;; automatically ensure every package exists (like :ensure or :straight)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://www.reddit.com/r/emacs/comments/12tj72z/nolittering_could_cause_backups_of_files/jh6i6oy/
(defun file-is-root-p (&optional name)
  "Check whether tramp su/sudo method is used for opening filepath NAME."
  ;; Adopted from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
  (let ((remote-method (file-remote-p (or name default-directory) 'method))
        (remote-user (file-remote-p (or name default-directory) 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

(defun file-is-not-root-p (&optional name)
  "Check whether file given by NAME is not a root file."
  (not (file-is-root-p name)))

(use-package s)

(defvar my-backup-directory "~/.emacs.d/var/backups/")
(if (not (file-directory-p my-backup-directory))
    (make-directory my-backup-directory t))

;; backup and tmp files
(setq backup-directory-alist `(("." . ,my-backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      kept-new-versions 200
      kept-old-versions 0
      delete-old-versions t
      vc-make-backup-files t
      version-control t)
(setq create-lockfiles nil) ;; can't set to a different directory so disable :/

(defun force-backup-of-buffer ()
  "Force backup of saved buffer."
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

;; handle bug where backups are created for sensitive files like pass edit
;; https://www.reddit.com/r/emacs/comments/12tj72z/nolittering_could_cause_backups_of_files/
(defun my-backup-enable-predicate (name)
  "Backup enable predicate for a given NAME."
  (and (normal-backup-enable-predicate name)
       ;; don't save password files
       (not (s-starts-with? "/dev/shm" name))
       (not (s-contains? "password-store" name))
       (file-is-not-root-p name)))

(setq backup-enable-predicate #'my-backup-enable-predicate)

;; auto-saves
(defvar my-auto-save-directory "~/.emacs.d/var/auto-save/")
(if (not (file-directory-p my-auto-save-directory))
    (make-directory my-auto-save-directory t))

(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-directory t))
      ;; `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-"
      auto-save-default t
      auto-save-timeout 10
      auto-save-interval 100
      auto-save-no-message t
      delete-auto-save-files t)

;; disable auto-save on certain tramp profiles
(connection-local-set-profile-variables
 'no-remote-auto-save-profile
 '((buffer-auto-save-file-name . nil)
   (remote-file-name-inhibit-auto-save-visited . t)
   (remote-file-name-inhibit-auto-save . t)))

;; disable auto-save for specific protocols
(dolist (protocol '("sudo" "doas" "su" "sudoedit" "ssh"))
  (connection-local-set-profiles
   `(:application tramp :protocol ,protocol 'no-remote-auto-save-profile)))

;;; minimal-init.el ends here
