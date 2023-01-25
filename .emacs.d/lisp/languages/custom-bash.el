;;; custom-bash.el --- bash config
;;;
;;; Commentary:
;;;
;;; bash config
;;;
;;; Code:

;; on shell script save mave the file executable
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(provide 'custom-bash)
;;; custom-bash.el ends here
