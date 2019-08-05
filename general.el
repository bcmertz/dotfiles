;;;;;;;;;;;;;;;;;;; ALL MODES CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Ivy is a completion framework, similar to Helm. When downloading Ivy, it comes with Counsel and Swiper, which we'll get to in a minute. Ivy doesn't try to do too many things, instead it provides an interface to list, search, filter and perform actions on a collectionof "things". These "things" can range from strings to buffers, Ivy doesn't really care. It just provides a way for the user to interact with this collection.

;; Ergonomic Text Editing
(delete-selection-mode 1)                                 ; replace highlighted sections
(electric-pair-mode 1)                                    ; fill right

;; Navigation
(use-package avy
  :ensure t
  :bind ("M-g" . avy-goto-char-2)    ;; go to char
  ("M-l" . avy-goto-line))           ;; go to line
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  ("C-r" . swiper))

;; (use-package ag
;;   :ensure t
;;   :bind ("M-s" . ag))

;; Better File Searching
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-s") 'counsel-ag)

;; emacs window management

(define-key input-decode-map "\e\eOA" [(meta up)]) ;; because we're in a terminal a lot
(define-key input-decode-map "\e\eOB" [(meta down)])

(use-package windmove
  :ensure t
  :config
  ;; wrap around at edges
  ;; (setq windmove-wrap-around t)
  (windmove-default-keybindings 'meta))

;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)  ;; M-/
(global-set-key (kbd "C-_") 'undo)                         ;; C-/ in reality
(global-set-key (kbd "C-x C-e") 'eval-buffer)              ;; useful for editing init.el et al


(defun return-newline-below ()                             ;; go to end of line and return
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") 'return-newline-below)      ;; TTy C-m and RET is weird


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

(global-set-key [(control up)]  'move-line-up)            ;; C-up   moves current line up
(global-set-key [(control down)]  'move-line-down)        ;; C-down move current line down



;; Keystroke Completion
(use-package which-key
	 :ensure t
	 :config
	 (which-key-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("C-c M-." . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines)
  )

;; Buffer Management
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)              ; Better C-x b

(defalias 'list-buffers 'ibuffer)    ; better C-x C-b 

;; Ido like M-x command completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  )

;; Sidebar File Viewer
;; C-c C-c makes the focused directory the new root view
(require 'neotree)
(global-set-key (kbd "C-\\") 'neotree-toggle)


;; Git integration
(global-set-key (kbd "C-x g") 'magit-status)
