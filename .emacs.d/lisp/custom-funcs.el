;;; custom-funcs.el --- utilities and custom defined functions
;;;
;;; Commentary:
;;;
;;; custom funcs
;;;
;;; Code:
;; select current line
(defun mark-entire-line ()
  "mark the whole line from the indent to the end"
  (interactive)
  (beginning-of-line-text)
  (set-mark-command nil)
  (end-of-line))


(defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir (ignore-errors   ;;; Pick one: projectile or find-file-in-project
           ; (projectile-project-root)
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))

(defun return-newline-below ()
  "Go to end of line and return bc in TTy C-m and RET is weird."
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down)))

;; delete don't kill backwards for M-del
(defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
    (interactive "p")
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
    (interactive "p")
    (delete-word (- arg)))

;;; custom-funcs.el ends here
