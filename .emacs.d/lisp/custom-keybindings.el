;;; custom-keybindings.el --- set evil mode keymap -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; toggle with M-SPC; leader key is SPC
;;;
;;; Code:

;; https://sqrtminusone.xyz/configs/emacs/#variables-and-environment
(defun my/dump-bindings-recursive (prefix &optional level buffer)
  "Dump keybindings starting with PREFIX in a tree like form.
Optionally specify tree depth with LEVEL and current BUFFER."
  (dolist (key (which-key--get-bindings (kbd prefix)))
    (with-current-buffer buffer
      (when level
	(insert (make-string level ? )))
      (insert (apply #'format "%s%s%s\n" key)))
    (when (string-match-p
	   (rx bos "+" (* nonl))
	   (substring-no-properties (elt key 2)))
      (my/dump-bindings-recursive
       (concat prefix " " (substring-no-properties (car key)))
       (+ 2 (or level 0))
       buffer))))

(defun my/dump-bindings (prefix)
  "Dump keybindings starting with PREFIX in a tree-like form."
  (interactive "sPrefix: ")
  (let ((buffer (get-buffer-create "bindings")))
    (with-current-buffer buffer
      (erase-buffer))
    (my/dump-bindings-recursive prefix 0 buffer)
    (with-current-buffer buffer
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

(use-package bind-key
  :defer t
  :straight (:type built-in))

(use-package transient
  :defer t
  :straight (:type built-in))

;; Keystroke Completion
(use-package which-key
  :straight (:type built-in)
  :defer t
  :config
  ;;(define-key which-key-mode-map (kbd "<next>") 'which-key-show-next-page-cycle)
  ;;(define-key which-key-mode-map (kbd "<prior>") 'which-key-show-previous-page-cycle)
  ;; (setq which-key-paging-prefixes '())
  ;; (setq which-key-paging-key "C-h")
  ;; (setq which-key-popup-type 'side-window)
  ;; (setq which-key-side-window-max-height 0.66)
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package repeat
  :straight (:type built-in)
  :defer t
  :init
  (repeat-mode))

;; terminal specific escape codes
;; found with showkey -a
(add-hook 'tty-setup-hook
          (lambda ()
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
             (define-key input-decode-map "\e[1;7A" [(C-M-up)])
             (define-key input-decode-map "\e[1;7B" [(C-M-down)])
             (define-key input-decode-map "\e[1;7C" [(C-M-right)])
             (define-key input-decode-map "\e[1;7D" [(C-M-left)])
             (define-key input-decode-map "\e[5~" (kbd "<prior>"))
             (define-key input-decode-map "\e[6~" (kbd "<next>"))
             ;; quick and dirty way to bind C-/ to undo in terminal
             (global-set-key (kbd "C-_") 'undo)))

(if (equal "st-meta-256color" (tty-type))
    (define-key input-decode-map "\e[1;2A\" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map \"\e[1;2A\" [S-up]))]))]"))


(use-package evil
  :defer t)

(use-package general
  :config
  (general-override-mode 1))
;;   (general-create-definer tyrant-def
;;     :states '(normal visual insert motion emacs)
;;     :prefix "SPC"
;;     :non-normal-prefix "C-SPC")
;;   (tyrant-def
;;     "" nil
;;     "h"   (general-simulate-key "C-h" :which-key "help")
;;     "u"   (general-simulate-key "C-u" :which-key "u")
;;
;;     "SPC" '(avy-goto-char :which-key "goto char")
;;     "j" '(avy-goto-line :which-key "goto line")
;;     "p"  (general-simulate-key "C-c p" :which-key "projectile")
;;     "s" 'swiper
;;     "\\" '(neotree-project-dir-toggle :which-key "neotree")
;;     ";" '(er/expand-region :which-key "expand")
;;     "m" '(:ignore t :which-key "mode")
;;
;;     "x"  '(:ignore t :which-key "file")
;;     "xf" 'counsel-find-file
;;     "xs" 'save-buffer
;;     "xS" 'save-some-buffers
;;     "xa" 'counsel-ag
;;
;;     "w"  '(:ignore t :which-key "window")
;;     "w=" 'enlarge-window-horizontally
;;     "w+" 'enlarge-window
;;     "w-" 'shrink-window-horizontally
;;     "w_" 'shrink-window
;;     "wo" 'other-window
;;     "w0" 'delete-window
;;     "w1" 'delete-other-windows
;;     "w2" 'split-window-below
;;     "w3" 'split-window-right
;;
;;     "t"  '(:ignore t :which-key "theme")
;;     "th" 'global-hl-line-mode
;;     "tn" 'global-display-line-numbers-mode
;;     "tl" 'load-theme
;;     "td" 'disable-theme
;;
;;     "f"  '(:ignore t :which-key "folding")
;;     "ff" 'vimish-fold
;;     "fd" 'vimish-fold-delete
;;     "fl" 'vimish-fold-avy
;;     "fD" 'vimish-fold-delete-all
;;
;;     "c"  '(:ignore t :which-key "multicurse")
;;     "c." 'mc/mark-next-like-this
;;     "c," 'mc/mark-previous-like-this
;;     "ca" 'mc/mark-all-like-this
;;     "ce" 'mc/edit-lines
;;
;;     ;; Buffer operations
;;     "b"   '(:ignore t :which-key "buffer")
;;     "bk"  'kill-this-buffer
;;     "bK"  'kill-buffer-and-window
;;     "b]"  'next-buffer
;;     "b["  'previous-buffer
;;     "bR"  'rename-file-and-buffer
;;     "br"  'revert-buffer
;;
;;     ;; Applications
;;     "a" '(:ignore t :which-key "Applications")
;;     "ag" 'magit-status
;;     "ad" 'dired
;;
;;     ;; SmartParens
;;     "e" '(:ignore t :which-key "SmartParens")
;;     "ee" 'sp-show-enclosing-pair
;;     "eu" 'sp-up-sexp
;;     "ed" 'sp-down-sexp
;;     "ei" 'sp-change-enclosing
;;     "ec" 'sp-rewrap-sexp))

(global-set-key (kbd "M-SPC") 'evil-mode)

(provide 'custom-keybindings)
;;; custom-keybindings.el ends here
