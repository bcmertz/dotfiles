;;; custom-editing.el --- configuration pertaining to editing text -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; code folding, syntax based packages, multiple cursors, etc
;;;
;;; Code:

(use-package util-funcs
  :straight (util-funcs :type nil :local-repo "~/.emacs.d/lisp/lib/util-funcs/"))

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)
;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setq comment-empty-lines t)

;; xref navigate definitions
(use-package xref
  :straight (:type built-in)
  :bind (:map prog-mode-map
              ("C-." . xref-find-definitions)
              ("C-M-." . xref-find-definitions-other-window)
              ("C-," . xref-go-back)
              ("C-M-," . xref-go-forward)))

;; code navigation
(use-package avy
  :bind (("M-g" . avy-goto-char)    ;; go to char
         ("M-l" . avy-goto-line)))  ;; go to line

;; zap to char using avy
(use-package avy-zap :defer t)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

;; run comment-line instead of comment-dwim
;; http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
;; I switched this out because comment-dwim breaks lines instead of commenting each line
;; in a region like comment-line does
(global-set-key (kbd "M-;") 'comment-line)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; untabify on save
;; (add-hook 'before-save-hook '(lambda()
;;                                (untabify (point-min) (point-max))))

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda()
                               (when (not (or (derived-mode-p 'markdown-mode 'org-mode)))
                                 (delete-trailing-whitespace))))

;; kills the entire line plus the newline whenever you invoke kill-line (i.e. via C-k).
(setq kill-whole-line t)

;; make C-w with no selection kill rest of line
;;
;; modified from https://www.masteringemacs.org/article/whats-new-in-emacs-311
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (point)
           (line-end-position)))))

;; sexpr wrapping
(use-package smartparens
  :defer t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (add-hook 'org-mode-hook (lambda () (setq-local smartparens-global-mode nil)))

  (show-smartparens-global-mode +1)
  (add-hook 'org-mode-hook (lambda () (setq-local show-smartparens-global-mode nil)))

  (which-key-add-key-based-replacements "C-c s" "smartparens")
  (global-set-key (kbd "C-c s e") 'sp-show-enclosing-pair)
  (global-set-key (kbd "C-c s u") 'sp-up-sexp)
  (global-set-key (kbd "C-c s d") 'sp-down-sexp)
  (global-set-key (kbd "C-c s i") 'sp-change-enclosing)
  (global-set-key (kbd "C-c s c") 'sp-rewrap-sexp))

(defun set-sp-face ()
  "Customize matching sp face."
  (add-hook 'smartparens-mode-hook
	    (lambda ()
              (set-face-attribute 'sp-show-pair-match-face nil
                                  :foreground "green"
                                  :background 'unspecified
                                  :weight 'normal
                                  :underline nil ;; "#16A085"
                                  ))))
(apply-if-gui 'set-sp-face)

;; From smartparens documentation
(sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; give option to diff modified file before killing it
;; https://emacs.stackexchange.com/questions/3245/kill-buffer-prompt-with-option-to-diff-the-changes/3363#3363
(defun my/save-or-diff-buffer ()
  "When an unsaved buffer is kill give the option to diff it first."
  (catch 'quit
    (save-window-excursion
      (let ((done nil)
            (kill t))
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q)
                           (setq-local kill nil)
                           (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        kill))))

(add-to-list 'kill-buffer-query-functions #'my/save-or-diff-buffer t)

;; Utilities
(global-set-key (kbd "C-x C-e") 'eval-buffer)

(defun return-newline-below ()
  "Go to end of line and return bc in TTy C-m and RET is weird."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") 'return-newline-below)     ;; return new line below

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

;; use 'transpose-regions for region marking
(global-set-key [(control up)] 'move-lines-up)
(global-set-key [(control down)] 'move-lines-down)

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

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key [C-backspace] 'backward-delete-word)
(global-set-key [C-delete] 'forward-delete-word)

;; rename file and buffer
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;; builtin autoreverting
(use-package autorevert
  :straight (:type built-in)
  :config
  (setq auto-revert-interval 0.25)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose t)
  (global-auto-revert-mode +1))

(use-package highlight-changes
  :straight (:type built-in)
  :defer t)

(use-package highlight-unsaved
  :after highlight-changes
  :straight (highlight-unsaved :type nil :local-repo "~/.emacs.d/lisp/lib/highlight-unsaved"))

(provide 'custom-editing)
;;; custom-editing.el ends here
