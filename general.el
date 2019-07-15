;;;;;;;;;;;;;;;;;;; ALL MODES CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ergonomic Text Editing
(delete-selection-mode 1)                                 ; replace highlighted sections
(electric-pair-mode 1)                                    ; fill right

;; Navigation
(use-package avy
  :ensure t
  :bind ("M-g" . avy-goto-char-2)) ; ("M-g" . avy-goto-line) - not that useful compared
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  ("C-r" . swiper))
(use-package ag
  :ensure t
  :bind ("M-s" . ag))

;; Better File Searching
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Emacs Window Management
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    ))

;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C-x C-e") 'eval-buffer)

;; Keystroke Completion
(use-package which-key
	 :ensure t
	 :config
	 (which-key-mode))

;; Buffer Management
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)              ; Better C-x b

(defalias 'list-buffers 'ibuffer)    ; better C-x C-b 

;; Sidebar File Viewer
;; C-c C-c makes the focused directory the new root view
(require 'neotree)
(global-set-key (kbd "C-\\") 'neotree-toggle)

(global-set-key (kbd "C-x g") 'magit-status)
