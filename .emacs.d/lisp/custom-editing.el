;;; custom-editing.el --- configuration pertaining to editing text
;;;
;;; Commentary:
;;;
;;; code folding, syntax based packages, multiple cursors, etc
;;;
;;; Code:
;; zap to char using avy
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

;; run comment-line instead of comment-dwim
;; http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
;; I switched this out because comment-dwim breaks lines instead of commenting each line
;; in a region like comment-line does
(global-set-key (kbd "M-;") 'comment-line)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; replace highlighted sections
(delete-selection-mode 1)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/web-mode")
(yas-global-mode 1)

;; smart parenthesis
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode +1)
(global-set-key (kbd "C-c s e") 'sp-show-enclosing-pair)
(global-set-key (kbd "C-c s u") 'sp-up-sexp)
(global-set-key (kbd "C-c s d") 'sp-down-sexp)
(global-set-key (kbd "C-c s i") 'sp-change-enclosing)
(global-set-key (kbd "C-c s c") 'sp-rewrap-sexp)

;; code folding
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c f f") #'vimish-fold)
(global-set-key (kbd "C-c f d") #'vimish-fold-delete)
(global-set-key (kbd "C-c f l") #'vimish-fold-avy) ;; fold to line
(global-set-key (kbd "C-c f t") #'vimish-fold-toggle) ;; fold to line
(global-set-key (kbd "M-`") #'vimish-fold-delete-all)

;; text selection
(use-package expand-region
  :bind("C-;" . er/expand-region)
  )

;; Undo tree
(use-package undo-tree
  :bind
  ;; hack to avoid conflict
  ("C-_" . text-scale-decrease)
  :config
  (global-undo-tree-mode))

;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-x C-e") 'eval-buffer)
(if (display-graphic-p)  ;; use after init hook for it to work for some reason
    (add-hook 'after-init-hook (lambda ()
                                 (global-unset-key (kbd "C-_"))
                                 (global-set-key (kbd "C-+") 'text-scale-increase)
                                 (global-set-key (kbd "C-_") 'text-scale-decrease)
                                 )))

(global-set-key (kbd "C-x C-l") 'mark-entire-line)

(global-set-key (kbd "<M-RET>") 'return-newline-below)     ;; return new line below

(global-set-key [(control up)] 'move-lines-up)
(global-set-key [(control down)] 'move-lines-down)

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
:bind
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("C-c M-." . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines)
  )

;; Keystroke Completion
(use-package which-key
         :ensure t
         :config
         (which-key-mode))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; rename file and buffer
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;;; custom-editing.el ends here
