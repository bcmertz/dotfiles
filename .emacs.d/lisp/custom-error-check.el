;;; custom-error-check.el --- syntax checking -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; syntax checking
;;;
;;; Code:

(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)
              ("C-c e s" . flymake-start)
              ("C-c e i" . flymake-show-diagnostic)
              ("C-c e l" . consult-flymake)
              ("C-c e L" . flymake-show-buffer-diagnostics)
              ("<left-fringe> <mouse-1>" . nil)
              ("C-c e m" . flymake-mode))
  :config
  (which-key-add-key-based-replacements "C-c e" "errors"))

;; TODO: WIP collect all diagnostics from project root / user specified dir
(defun my/check-all-files-in-project ()
  (interactive)
  (let* ((dir (read-directory-name "Select directory: " (current-project-root)))
         (check-dir (file-name-as-directory dir)))
    (dolist (file (directory-files-recursively check-dir
                                               "\\`[^\\.].*\\'"
                                               nil
                                               t))
      (message "%s" file)
      (message "%s" (file-name-extension file))

      (with-temp-buffer
        ;; (save-excursion
        ;; (find-file file)
        (insert-file-contents file t)
        (python-mode)
        ;; (message "%s" major-mode)
        ;; (eglot-ensure)
        (flymake-mode 1)
        (flymake-start)
        (message "%s" (flymake-diagnostics)) ;; ?? this doesn't work but is the function which collects diagnostics in open buffers successfully
        )
      )
    ))


(provide 'custom-error-check)
;;; custom-error-check.el ends here
