;;; custom-splashscreen.el --- splashscreen
;;;
;;; Commentary:
;;;
;;; custom scratch splashscreen
;;;
;;; Code:

(defvar splash-type "image" "String type of splash-screen to use.
Possible values are `cowsay' and `image'")

;; Inital buffer
(setq inhibit-startup-screen t)  ;; inhibit startup screen
(setq initial-buffer-choice t)   ;; scratch instead

;; customize scratch buffer
(defun custom-get-scratch-buffer-create ()
  "Create the *scratch* buffer."
  (let ((scratch (get-buffer-create "*scratch*")))
    (with-current-buffer "*scratch*"
      (delete-region (point-min) (point-max))
      (if (string= splash-type "image")
          (progn
            (setq-local imgs (directory-files "~/.emacs.d/etc/" t directory-files-no-dot-files-regexp))
            (insert-image (create-image (nth (random (length imgs)) imgs)))
            (newline)))
      (when initial-scratch-message
        (insert (substitute-command-keys initial-scratch-message))
        (set-buffer-modified-p nil))
      (funcall initial-major-mode)
      (flyspell-mode -1))
    scratch))

(defun add-scratch-advice ()
  "Add scratch advice to insert image."
  (advice-add 'get-scratch-buffer-create :override #'custom-get-scratch-buffer-create))
(apply-if-gui 'add-scratch-advice)

(defun return-greeting ()
  "Return time based greeting."
  (let ((str "")
        (hour (string-to-number (nth 0 (split-string (nth 3 (split-string (current-time-string))) ":")))))
    (if (< hour 5)
        (setq str "Good evening")
      (if (< hour 12)
          (setq str "Good morning")
        (if (< hour 17)
            (setq str "Good afternoon")
          (setq str "Good evening"))))))

(setq initial-scratch-message (format "%s
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To execute lisp, type C-x C-e

" (if (string= splash-type "image")
      (format ";; %s, %s" (return-greeting) (capitalize (user-full-name)))
    (if (string= splash-type "cowsay")
        ;; regexp_1 comments out lines, regexp_2 strips eol whitespace
        (replace-regexp-in-string "\s+$" "" (replace-regexp-in-string "^" ";; " (shell-command-to-string
                                                                                 (format "cowsay -f udder %s, %s"
                                                                                         (return-greeting) (capitalize (user-full-name))))))))))

(defun set-gui-scratch-greeting (msg)
  "Set GUI scratch greeting."
  (setq initial-scratch-message msg))

(apply-if-gui 'set-gui-scratch-greeting initial-scratch-message)


(provide 'custom-splashscreen)
;;; custom-splashscreen.el ends here
