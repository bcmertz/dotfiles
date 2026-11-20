;;; util-funcs.el --- utilities and custom defined functions -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; custom funcs
;;;
;;; Code:

;; if gui do something in whatver type of emacs instance we are using
(defmacro if-gui (action &rest action2)
  "Do specified ACTION if we're in a gui regardless of daemon or not and if not ACTION2."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (select-frame frame)
                   (if (display-graphic-p frame)
                       ,action
                     ,@action2)))
     (if (display-graphic-p)
         ,action
       ,@action2)))

;; if gui do something in whatver type of emacs instance we are using
(defun apply-if-gui (&rest action action2)
  "Do specified ACTION if we're in a gui regardless of daemon or not and if not ACTION2."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (display-graphic-p frame)
                      (apply action)
                    (if action2 (apply action2)))))
    (if (display-graphic-p)
        (apply action)
      (if action2 (apply action2)))))

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

(defun current-project-root ()
  "Return current project root dir or nil."
  (ignore-errors (project-root (project-current))))

;; https://kisaragi-hiu.com/emacs-completion-metadata.html
;; utility function to give a completing read instance a new completion category
;; that we can use vertico-multiform or marginalia to treat differently
;;
;; Example usage:
;; (completing-read "Prompt: "
;;                 (my-mark-category '("/usr" "/tmp" "/home") 'dir))
;;
(defun my-mark-category (seq category)
  "Mark SEQ as being in CATEGORY."
  (lambda (str pred flag)
    (pcase flag
      ('metadata
       `(metadata (category . ,category)))
      (_
       (all-completions str seq pred)))))

(defun get-theme-variable-from-palette (var)
  "Get theme VAR from palette."
  (let (color)
    (if (string-match-p (regexp-quote "doom") (format "%s" (car custom-enabled-themes)))
        (setq color (doom-darken (doom-color var) 0.05))
      (if (string-match-p (regexp-quote "ef-") (format "%s" (car custom-enabled-themes)))
          (setq color (ef-themes-get-color-value (car custom-enabled-themes)))
        (if (string-match-p (regexp-quote "atom-one-dark") (format "%s" (car custom-enabled-themes)))
            ;; need to test for various special names
            (if (string= var "bg-alt")
                (setq color (nth 4 (assoc "atom-one-dark-bg-1" atom-one-dark-colors-alist)))))))
    (if (not color)
        (setq color 'unspecified))
    color))

(defun modified-buffers-exist-p ()
  "This function will check to see if there are any buffers\
that have been modified. It will return true if there are
and nil otherwise. Buffers that have `buffer-offer-save' set to
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

;; https://www.emacswiki.org/emacs/EmacsAsDaemon#h5o-10
(defun client-save-kill-emacs (&optional display)
  "This is a function that can bu used to save buffers and\
shutdown the Emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'. This function will
check to see if there are any modified buffers, active clients
or frame. If so, an x window will be opened and the user will
be prompted. DISPLAY allows user to specify where to create
any new frames that are needed."
  (let (new-frame modified-buffers active-clients-or-frames)
    ;; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist-p))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
				     (> (length (frame-list)) 1)))
    ;; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))
    ;; Save the current frame.
    (setq new-frame (selected-frame))
    ;; When displaying the number of clients and frames:
    ;; subtract 1 from clients (this client).
    ;; subtract 2 from frames (the frame just created and the default frame.)
    (when (or (not active-clients-or-frames)
	      (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))
      ;; If the user quits during the save dialog then don't exit emacs.
      ;; Still close the terminal though.
      (let((inhibit-quit t))
        ;; Save buffers
	(with-local-quit
	  (save-some-buffers))
	(if quit-flag
	    (setq quit-flag nil)
          ;; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
            ;; Exit emacs
	    (kill-emacs)))))
    ;; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))))

;; https://www.emacswiki.org/emacs/misc-cmds.el
(defun kill-buffer-and-its-windows (buffer &optional msgp)
  "Kill BUFFER and delete its windows. Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string). MSGP optional."
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
    (when msgp (error "Cannot kill buffer. Not a live buffer: `%s'" buffer))))

;; https://emacs.stackexchange.com/a/13275
(defun run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always run in the `current-buffer'.
Cancels itself, if this buffer was killed. Takes same arguments of SECS,
REPEAT, FUNCTION, and rest of ARGS."
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

;; return or comment the percent of a file that is comments and docstrings
;; modified from https://github.com/radian-software/straight.el?tab=readme-ov-file#comments-and-docstrings
(defun percent-comments ()
  "Find the percent of a file that is comments and docstrings."
  (interactive)
  (let ((lines (make-hash-table :test #'equal)))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (when (memq (face-at-point)
                  '(font-lock-comment-face
                    font-lock-doc-face))
        (puthash (line-number-at-pos) t lines))
      (forward-char))
    (setq per (* (/ (float (length (hash-table-keys lines)))
                    (line-number-at-pos))
                 100))
    (if (called-interactively-p 'interactive)
        (message "%s" per)
      per)))

(defun if-in-project (action_1 &optional action_2)
  "If in a project, do ACTION_1 else ACTION_2."
  (let ((project-dir (current-project-root)))
    (if project-dir
        (apply action_1)
      (if action_2
          (apply action_2)))))

;; move file
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
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location) ;; removes from cache
      (setq recentf-list (delete old-location recentf-list)))))

;; https://www.emacswiki.org/emacs/misc-cmds.el
(defun quit-window-delete (&optional window)
  "Quit WINDOW, deleting it, and bury its buffer.
WINDOW must be a live window and defaults to the selected one.
With prefix argument KILL non-nil, kill the buffer instead of
burying it.

This is similar to the version of `quit-window' that Emacs had before
the introduction of `quit-restore-window'. It ignores the information
stored in WINDOW's `quit-restore' window parameter.

It deletes the WINDOW more often, rather than switching to another
buffer in it. If WINDOW is alone in its frame then the frame is
deleted or iconified, according to option `frame-auto-hide-function'."
  (interactive "P")
  (set-window-parameter window 'quit-restore `(frame frame nil ,(current-buffer)))
  (quit-restore-window window 'kill))

;; https://emacs.stackexchange.com/a/35072
;; mark whole word
(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but select ARG whole words and skips over whitespace.
If you use a negative prefix ARG then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument (Negative NUM
selects backward).

When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
set ARG words from point."
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

(defun silence-function-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN.
Pass additional ARGS to function call."
  (let ((inhibit-message t)      ;Don't show the messages in Echo area
        (message-log-max nil))   ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(provide 'util-funcs)
;;; util-funcs.el ends here
