;;;;;;;;;;;;;;;;  ALL MODES CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages and repository management
(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-1" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa-2" . "https://melpa.org/packages/") t)

(setq initial-buffer-choice "~/todo.org")


(require 'evil)
(evil-mode 1)

(use-package general
  :ensure t
  :config
  (general-override-mode 1) 
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "c"   (general-simulate-key "C-c" :which-key "c")
    "h"   (general-simulate-key "C-h" :which-key "help")
    "u"   (general-simulate-key "C-u" :which-key "u")

    "SPC" '(avy-goto-char :which-key "goto char")
    "j" '(avy-goto-line :which-key "goto line")
    "p"  (general-simulate-key "C-c p" :which-key "projectile")   
    "s" 'swiper
    "\\" '(neotree-project-dir-toggle :which-key "neotree")
    ";" '(er/expand-region :which-key "expand")
    "e" '(evil-mode :which-key "evil")

    "x"  '(:ignore t :which-key "file")
    "xf" 'counsel-find-file
    "xs" 'save-buffer
    "xS" 'save-some-buffers
    "xa" 'counsel-ag

    "w"  '(:ignore t :which-key "window")
    "w=" 'enlarge-window-horizontally
    "w+" 'enlarge-window
    "w-" 'shrink-window-horizontally
    "w_" 'shrink-window
    "wo" 'other-window
    "w0" 'delete-window
    "w1" 'delete-other-windows
    "w2" 'split-window-below
    "w3" 'split-window-right

    "t"  '(:ignore t :which-key "theme")
    "th" 'global-hl-line-mode
    "tn" 'global-display-line-numbers-mode
    "tl" 'load-theme
    "td" 'disable-theme

    "f"  '(:ignore t :which-key "folding")
    "ff" 'vimish-fold
    "fd" 'vimish-fold-delete
    "fl" 'vimish-fold-avy
    "fD" 'vimish-fold-delete-all

    "m"  '(:ignore t :which-key "multicurse")
    "m." 'mc/mark-next-like-this
    "m," 'mc/mark-previous-like-this
    "ma" 'mc/mark-all-like-this
    "me" 'mc/edit-lines
 
    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bk"  'kill-this-buffer
    "bK"  'kill-buffer-and-window
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer   
    
    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ag" 'magit-status
    "ad" 'dired))

(global-set-key (kbd "M-SPC") 'evil-mode)

;; Follow symlinks
(setq vc-follow-symlinks t)
;; use y/n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; terminal specific escape codes
(add-hook 'tty-setup-hook
          '(lambda ()
             (define-key input-decode-map "\e[1;3A" [M-up])
             (define-key input-decode-map "\e[1;3B" [(meta down)])
             (define-key input-decode-map "\e[1;3C" [(meta right)])
             (define-key input-decode-map "\e[1;3D" [(meta left)])
             (define-key input-decode-map "\e[1;2A" [S-up])
             (define-key input-decode-map "\e[1;2B" [(shift down)])
             (define-key input-decode-map "\e[1;2C" [(shift right)])
             (define-key input-decode-map "\e[1;2D" [(shift left)])
             (define-key input-decode-map "\e[1;5A" [(C-up)])
             (define-key input-decode-map "\e[1;5B" [(control down)])
             (define-key input-decode-map "\e[1;5C" [(control right)])
             (define-key input-decode-map "\e[1;5D" [(control left)])
             ))
(if (equal "st-meta-256color" (tty-type))
        (define-key input-decode-map "\e[1;2A\" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map \"\e[1;2A\" [S-up]))]))]"))

;; Keystroke Completion
(use-package which-key
         :ensure t
         :config
         (which-key-mode))

;; Git integration
(global-set-key (kbd "C-x g") 'magit-status)

;;; custom-general.el ends here
