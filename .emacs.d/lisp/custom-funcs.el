;;; custom-funcs.el --- utilities and custom defined functions -*- lexical-binding:t -*-
;;;
;;; Commentary:
;;;
;;; custom funcs
;;;
;;; Code:

;; refresh ewal theme
(defun refresh-theme ()
  "Refresh the theme based on global config of whether to use pywal colors for Emacs."
  (interactive)
  (if (string= (shell-command-to-string "global_cfg pywal_emacs") "true")
      (setq custom-theme 'ewal-doom-one)
    (setq custom-theme default-theme))
  ;; disable all enabled themes
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  ;; load theme
  (load-theme custom-theme))

;; if gui do something in whatver type of emacs instance we are using
(defun apply-if-gui (&rest action)
  "Do specified ACTION if we're in a gui regardless of daemon or not."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p frame)
                      (apply action))))
    (if (display-graphic-p)
        (apply action))))

;; select current line
(defun mark-entire-line ()
  "Mark the whole line from the indent to the end."
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
  (newline-and-indent)
  ;; (if (string= major-mode "web-mode")
  ;;     (indent-relative)
  ;;     )
  )

(defun org-return-newline-below ()
  "Go to end of line and org-meta-return."
  (interactive)
  (end-of-line)
  (if (string= major-mode "org-mode")
      (org-meta-return)
      (progn
        (newline-and-indent)
        (indent-relative)
        )
      )
  )


(defun move-lines (n)
  (let ((beg) (end) (keep))
    (if mark-active
        (save-excursion
          (setq keep t)
          (setq beg (region-beginning)
                end (region-end))
          (goto-char beg)
          (setq beg (line-beginning-position))
          (goto-char end)
          (setq end (line-beginning-position 2)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))
    (let ((offset (if (and (mark t)
                           (and (>= (mark t) beg)
                                (< (mark t) end)))
                      (- (point) (mark t))))
          (rewind (- end (point))))
      (goto-char (if (< n 0) beg end))
      (forward-line n)
      (insert (delete-and-extract-region beg end))
      (backward-char rewind)
      (if offset (set-mark (- (point) offset))))
    (if keep
        (setq mark-active t
              deactivate-mark nil))))

(defun move-lines-up (n)
  "Move the line(s) spanned by the active region up by N lines."
  (interactive "*p")
  (move-lines (- (or n 1))))

(defun move-lines-down (n)
  "Move the line(s) spanned by the active region down by N lines."
  (interactive "*p")
  (move-lines (or n 1)))

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

(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
            (delete-file old-location))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let (
        (name (buffer-name))
        (filename (buffer-file-name))
        )
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; source https://emacs.stackexchange.com/questions/46664/switch-between-horizontal-and-vertical-splitting
(defun toggle-window-split ()
  "Toggle window split orientation."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))


;; start bash script and enable shell mode
(defun start-shell-script ()
  "Start shell script by inserting header and entering shell mode."
  (interactive)
  (insert "#!/bin/sh\n#\n# description")
  (shell-script-mode))


;; Turn org text into link and follow it to corresponding header
(defun org-open-at-point-plaintext ()
  "Turn org text into link and follow it to corresponding header."
  (interactive)
  (if (string= " " (string (preceding-char)))
      (insert "[[")
    (progn
      (backward-word)
      (insert "[["))
    )
  (forward-word)
  (insert "]]")
  (backward-word)
  (org-open-at-point)
  (org-cycle)
  )

;; https://emacs.stackexchange.com/a/10714
(defun org-remove-link ()
  "Replace an org link by its description or if empty its address."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun bcm/org-return ()
  "Org return and if header is closed cycle it."
  (interactive)
  (org-return)
  (if (org-folded-p)
      (org-cycle)
      )
  )

;; https://emacs.stackexchange.com/a/26840
(defun org-folded-p ()
  "Return non-nil if point is on a folded headline or plain list item."
  (and (or (org-at-heading-p)
           (org-at-item-p))
       (invisible-p (point-at-eol))))

;; https://emacs.stackexchange.com/a/35072
;; mark whole word
(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))


;;; custom-funcs.el ends here
