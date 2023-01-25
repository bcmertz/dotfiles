;;; custom-funcs.el --- utilities and custom defined functions -*- lexical-binding:t -*-
;;;
;;; Commentary:
;;;
;;; custom funcs
;;;
;;; Code:

;; useful funcs from doom-emacs used elsewhere in config
(require 'doom-lib)

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

;; do for both gui and term, useful if an action gets messed up my emacsclient
;; in gui, as happens for styling / faces / fonts
(defun apply-gui-and-term (&rest action)
  "Do specified ACTION for gui and term with correct daemon support."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p frame)
                      (apply action))))
    (apply action)))

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
    (defvar project-dir)
    (defvar file-name)
    (defvar neo-smart-open)
    (let ((project-dir (current-project))
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

(defun get-theme-variable-from-palette (var)
  "Get theme VAR from palette."
  (let (color)
    (if (string-match-p (regexp-quote "doom") (format "%s" (car custom-enabled-themes)))
        (setq color (doom-darken (doom-color var) 0.05))
      (if (string-match-p (regexp-quote "ef-") (format "%s" (car custom-enabled-themes)))
          (setq color (car (cdr (assoc var (ef-themes--palette-value (car custom-enabled-themes))))))))
    (if (not color)
        (setq color 'unspecified))
    color)
  )

;; https://www.emacswiki.org/emacs/EmacsAsDaemon#h5o-10
(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers, active clients
or frame.  If so, an x window will be opened and the user will
be prompted."
  (let (new-frame modified-buffers active-clients-or-frames)
    ; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
				     (> (length (frame-list)) 1)))

    ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

    ; Save the current frame.
    (setq new-frame (selected-frame))

    ; When displaying the number of clients and frames:
    ; subtract 1 from clients (this client).
    ; subtract 2 from frames (the frame just created and the default frame.)
    (when (or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

      ; If the user quits during the save dialog then don't exit emacs.
      ; Still close the terminal though.
      (let((inhibit-quit t))
             ; Save buffers
	(with-local-quit
	  (save-some-buffers))
	(if quit-flag
	  (setq quit-flag nil)
          ; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
		 ; Exit emacs
	    (kill-emacs)))
	))

    ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))))

(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0)))))
	(setq modified-found t)))
    modified-found))


;; https://www.emacswiki.org/emacs/misc-cmds.el
(defun kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive (list (read-buffer "Kill buffer: " (current-buffer) 'existing) 'MSGP))
  (setq buffer  (get-buffer buffer))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
      (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
        (when (and (buffer-modified-p buffer)
                   (fboundp '1on1-flash-ding-minibuffer-frame))
          (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
        (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
          (dolist (win  wins)           ; (User might keep buffer if modified.)
            (when (window-live-p win)
              ;; Ignore error, in particular,
              ;; "Attempt to delete the sole visible or iconified frame".
              (condition-case nil (delete-window win) (error nil))))))
    (when msgp (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;; https://emacs.stackexchange.com/a/13275
(defun run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.
Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))

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

(defun current-project ()
  "Return project-dir or nil."
  (ignore-errors   ;;; Pick one: projectile or find-file-in-project
    (or (projectile-project-root) (ffip-project-root))
    )
  )

(defun if-in-project (action_1 &optional action_2)
  "If in a project, do ACTION_1 else ACTION_2."
  (let ((project-dir (current-project)))
    (if project-dir
        (apply action_1)
      (if action_2
          (apply action_2)
        ))))

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

;; http://xahlee.info/emacs/emacs/emacs_kill-ring.html
(defun forward-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

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
    (if (y-or-n-p "File exists in destination, overwrite?")
        (delete-file new-location)
      (progn
        (message "Move file canceled")
        (return-from move-file))))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "Old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (if-in-project
     (projectile-cache-current-file))
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location) ;; removes from cache
      (setq recentf-list (delete old-location recentf-list)))))

;; from crux.el
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))


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
          (set-buffer-modified-p nil)
          (setq recentf-list (delete filename recentf-list))
          (if-in-project
           (progn
             (projectile-cache-current-file)
             (delete-file-projectile-remove-from-cache filename))))))))

;; (defun rename-file-and-buffer ()
;;   "Rename current buffer and if the buffer is visiting a file, rename it too."
;;   (interactive)
;;   (when-let* ((filename (buffer-file-name))
;;               (new-name (or (read-file-name "New name: " (file-name-directory filename) nil 'confirm)))
;;               (containing-dir (file-name-directory new-name)))
;;     ;; make sure the current buffer is saved and backed by some file
;;     (when (or (buffer-modified-p) (not (file-exists-p filename)))
;;       (if (y-or-n-p "Can't move file before saving it.  Would you like to save it now?")
;;           (save-buffer)))
;;     (if (get-file-buffer new-name)
;;         (message "There already exists a buffer named %s" new-name)
;;       (progn
;;         (make-directory containing-dir t)
;;         (cond
;;          ((vc-backend filename)
;;           ;; vc-rename-file seems not able to cope with remote filenames?
;;           (let ((vc-filename (if (tramp-tramp-file-p filename) (tramp-file-local-name filename) filename))
;;                 (vc-new-name (if (tramp-tramp-file-p new-name) (tramp-file-local-name filename) new-name)))
;;             (vc-rename-file vc-filename vc-new-name)))
;;          (t
;;           (rename-file filename new-name t)
;;           (set-visited-file-name new-name t t)))))))



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

(provide 'custom-funcs)
;;; custom-funcs.el ends here
