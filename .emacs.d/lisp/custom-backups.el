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

(provide 'custom-backups)
;;; custom-backups.el ends here
