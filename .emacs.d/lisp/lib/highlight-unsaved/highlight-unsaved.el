;;; highlight-unsaved.el --- highlight unsaved changes -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;
;;; modified from: https://karthinks.com/software/even-more-batteries-included-with-emacs/
;;;
;;; Code:

(defun highlight-changes-mode-turn-off ()
  "Toggle highlight changes mode."
  (and highlight-changes-mode (highlight-changes-mode -1)))

;;;###autoload
(define-minor-mode highlight-unsaved-mode
  "Highlight all changes until the buffer is saved."
  :lighter "H"
  (cond
   ((not (buffer-file-name))
    (user-error "Highlight-unsaved-mode is only meant for use in file-visiting buffers"))
   (highlight-unsaved-mode
    (highlight-changes-mode 1)
    (add-hook 'after-save-hook #'highlight-changes-mode-turn-on nil t)
    (add-hook 'before-save-hook #'highlight-changes-mode-turn-off nil t))
   (t (highlight-changes-mode -1)
      (remove-hook 'after-save-hook #'highlight-changes-mode-turn-on t)
      (remove-hook 'before-save-hook #'highlight-changes-mode-turn-off t))))

(provide 'highlight-unsaved)
;;; highlight-unsaved.el ends here
