;;; custom-backups.el --- configure backups and temporary files -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

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

;; handle bug where backups are created for sensitive files like pass edit
;; https://www.reddit.com/r/emacs/comments/12tj72z/nolittering_could_cause_backups_of_files/
;; TODO check how this evolves, check if custom-funcs used can be deduplicated with reopen-as-root.el
(defun my-backup-enable-predicate (name)
  "Backup enable predicate for a given NAME."
  (and (normal-backup-enable-predicate name)
       ;; don't save password files
       (not (s-starts-with? "/dev/shm" name))
       (not (s-contains? "password-store" name))
       (file-is-not-root-p name)))

(setq backup-enable-predicate #'my-backup-enable-predicate)

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

(provide 'custom-backups)
;;; custom-backups.el ends here