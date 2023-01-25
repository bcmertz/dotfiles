;;; reopen-as-root.el --- Reopen root files as root user
;;;
;;; Commentary:
;;;
;;; Code:

(require 'tramp)

(defgroup reopen-as-root nil
  "Reopen as root."
  :group 'convenience)

(defun already-root-p ()
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-user (file-remote-p default-directory 'user)))
    (and remote-method
         (or (member remote-method '("sudo" "su" "ksu" "doas"))
             (string= remote-user "root")))))

(defun find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening FILENAME as root."
  (let ((remote-method (file-remote-p default-directory 'method))
        (remote-host (file-remote-p default-directory 'host))
        (remote-localname (file-remote-p filename 'localname)))
    (find-alternate-file (format "/%s:root@%s:%s"
                                 (or remote-method (if (executable-find "doas")
						       "doas"
						     "sudo"))
                                 (or remote-host "localhost")
                                 (or remote-localname filename)))))

(defun file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.
See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (file-owner-uid filename)
         (user-uid)))

(defun reopen-as-root ()
  "Find file as root if necessary.

Meant to be used as `find-file-hook'.
See also `reopen-as-root-mode'."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (derived-mode-p 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (file-owned-by-user-p buffer-file-name))
    (find-alternate-file-as-root buffer-file-name)))

;;;###autoload
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (let ((remote-method (file-remote-p default-directory 'method))
            (remote-host (file-remote-p default-directory 'host))
            (remote-localname (file-remote-p default-directory 'localname)))
        (find-file (format "/%s:root@%s:%s"
                           (or remote-method (if (executable-find "doas")
						       "doas"
						     "sudo"))
                           (or remote-host "localhost")
                           (or remote-localname
                               (read-file-name "Find file (as root): ")))))

    (if (already-root-p)
        (message "Already editing this file as root.")
      (let ((place (point)))
        (find-alternate-file-as-root buffer-file-name)
        (goto-char place)))))

;;;###autoload
(define-minor-mode reopen-as-root-mode
  "Automatically reopen files as root if we can't write to them
as the current user."
  :lighter ""
  :global t
  (if reopen-as-root-mode
      (add-hook 'find-file-hook #'reopen-as-root)
    (remove-hook 'find-file-hook #'reopen-as-root)))

(provide 'reopen-as-root)

;;; reopen-as-root.el ends here
