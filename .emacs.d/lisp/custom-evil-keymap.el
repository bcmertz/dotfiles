;;; custom-evil-keymap.el --- set evil mode keymap
;;;
;;; Commentary:
;;;
;;; toggle with M-SPC; leader key is SPC
;;;
;;; Code:
(use-package evil
  :ensure t
  :defer t
  )

(use-package general
  :ensure t
  :config
  (general-override-mode 1)
  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (tyrant-def
    "" nil
    "h"   (general-simulate-key "C-h" :which-key "help")
    "u"   (general-simulate-key "C-u" :which-key "u")

    "SPC" '(avy-goto-char :which-key "goto char")
    "j" '(avy-goto-line :which-key "goto line")
    "p"  (general-simulate-key "C-c p" :which-key "projectile")
    "s" 'swiper
    "\\" '(neotree-project-dir-toggle :which-key "neotree")
    ";" '(er/expand-region :which-key "expand")
    "e" '(evil-mode :which-key "evil")
    "m" '(:ignore t :which-key "mode")

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

    "c"  '(:ignore t :which-key "multicurse")
    "c." 'mc/mark-next-like-this
    "c," 'mc/mark-previous-like-this
    "ca" 'mc/mark-all-like-this
    "ce" 'mc/edit-lines

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

;;; custom-evil-keymap.el ends here
