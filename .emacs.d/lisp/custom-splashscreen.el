;;; custom-splashscreen.el --- splashscreen
;;;
;;; Commentary:
;;;
;;; splashscreen
;;;
;;; Code:

;; Inital buffer
(setq inhibit-startup-screen t)  ;; inhibit startup screen
(setq initial-buffer-choice t)   ;; scratch instead

;; customize scratch buffer
(defun custom-get-scratch-buffer-create ()
  "Create the *scratch* buffer."
  (let ((scratch (get-buffer-create "*scratch*")))
    (with-current-buffer "*scratch*"
      (delete-region (point-min) (point-max))
      (setq-local imgs (directory-files "~/.emacs.d/etc/" t directory-files-no-dot-files-regexp))
      (insert-image (create-image (nth (random (length imgs)) imgs)))
      (when initial-scratch-message
        (insert (substitute-command-keys initial-scratch-message))
        (set-buffer-modified-p nil))
      (funcall initial-major-mode))
    scratch))

(defun add-scratch-advice ()
  "Add scratch advice to insert image."
  (advice-add 'get-scratch-buffer-create :override #'custom-get-scratch-buffer-create))
(apply-if-gui 'add-scratch-advice)

(defun return-greeting ()
  "Return time based greeting."
  (let ((str "")
        (hour (string-to-number (nth 0 (split-string (nth 3 (split-string (current-time-string))) ":")))))
    (if (< hour 12)
        (setq str "Good morning")
      (if (< hour 17)
          (setq str "Good afternoon")
        (setq str "Good evening")))))

(setq initial-scratch-message (format ";; %s, %s
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To execute lisp, type C-x C-e

" (return-greeting) (capitalize (user-full-name))))

;; cowsay example (close but not there, figure it out later if you want)
;; (message "%s" (shell-command "cowsay hi" (current-buffer))) ____


(defun set-gui-scratch-greeting ()
  "Set GUI scratch greeting."
  (setq initial-scratch-message (format "

;; %s, %s
;; To execute lisp, type C-x C-e

" (return-greeting) (capitalize (user-full-name)))))
(apply-if-gui 'set-gui-scratch-greeting)

(provide 'custom-splashscreen)
;;; custom-splashscreen.el ends here
