;;; custom-bash.el --- bash config  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; bash config
;;;
;;; Code:

;; start bash script and enable shell mode
(defun start-shell-script ()
  "Start shell script by inserting header and entering shell mode."
  (interactive)
  (insert "#!/bin/sh\n#\n# description")
  (shell-script-mode))

;; on shell script save mave the file executable
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(provide 'custom-bash)
;;; custom-bash.el ends here
