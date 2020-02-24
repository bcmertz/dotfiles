;;;;;;;;;;;;;;;;;;; ALL MODES CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Follow symlinks
(setq vc-follow-symlinks t)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; truncate long lines l/r horizontal scrolling
(set-default 'truncate-lines t)
(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
                                    (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
                                        (if truncate-lines (scroll-left 1))))

;; replace highlighted sections
(delete-selection-mode 1)

;; smart parenthesis
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode +1)
(global-set-key (kbd "C-<escape>") 'sp-show-enclosing-pair)
(global-set-key (kbd "M-<escape>") 'sp-up-sexp)
(global-set-key (kbd "C-M-<escape>") 'sp-down-sexp)

  

;; Navigation
(use-package avy
  :ensure t
  :bind ("M-g" . avy-goto-char-2)    ;; go to char
  ("M-l" . avy-goto-line))           ;; go to line
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  ("C-r" . swiper))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/coding/" "~/go/src/github.com/getlantern/"))

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

;; Sidebar File Viewer
;; C-c C-c makes the focused directory the new root view
(use-package neotree
  :ensure t
  :config
  ;; Type H to toggle hidden files
  (setq-default neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-autorefresh nil)
  (setq neo-window-fixed-size ())
  :bind("C-\\" . neotree-project-dir-toggle))

;; Better File Searching
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-s") 'counsel-ag)

;; emacs window management
(add-hook 'tty-setup-hook  ;; terminal specific escape codes
          '(lambda ()
             (define-key input-decode-map "\e[1;3A" [meta up])
             (define-key input-decode-map "\e[1;3B" [(meta down)])
             (define-key input-decode-map "\e[1;3C" [(meta right)])
             (define-key input-decode-map "\e[1;3D" [(meta left)])
             (define-key input-decode-map "\e[1;5A" [(control up)])
             (define-key input-decode-map "\e[1;5B" [(control down)])
             (define-key input-decode-map "\e[1;5C" [(control right)])
             (define-key input-decode-map "\e[1;5D" [(control left)])
             ))


(use-package windmove
  :ensure t
  :config
  ;; wrap around at edges
  ;; (setq windmove-wrap-around t)
  (windmove-default-keybindings 'meta))             ;; M-arrows to move

(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M--") 'shrink-window-horizontally)

;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)  ;; M-/
(global-set-key (kbd "C-_") 'undo)                         ;; C-/ in reality
(global-set-key (kbd "C-x C-e") 'eval-buffer)              ;; useful for editing init.el et al

(defun return-newline-below ()                             ;; go to end of line and return bc in TTy C-m and RET is weird
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<M-RET>") 'return-newline-below)     ;; return new line below

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

(global-set-key [(control up)] 'move-line-region-up)
(global-set-key [(control down)] 'move-line-region-down)


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


;; Git integration
(global-set-key (kbd "C-x g") 'magit-status)

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

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
