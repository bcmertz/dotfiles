;; custom editing config ;;;

;; zap to char using avy
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; replace highlighted sections
(delete-selection-mode 1)

;; smart parenthesis
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode +1)
(global-set-key (kbd "C-<escape>") 'sp-show-enclosing-pair)
(global-set-key (kbd "M-<escape>") 'sp-up-sexp)
(global-set-key (kbd "C-M-<escape>") 'sp-down-sexp)

;; code folding
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c f f") #'vimish-fold)
(global-set-key (kbd "C-c f d") #'vimish-fold-delete)
(global-set-key (kbd "C-c f l") #'vimish-fold-avy) ;; fold to line
(global-set-key (kbd "M-`") #'vimish-fold-delete-all)

;; text selection
(use-package expand-region
  :bind("C-;" . er/expand-region)
  )

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

(global-set-key [(control up)] 'move-line-region-up)
(global-set-key [(control down)] 'move-line-region-down)

;; Multiple Cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("C-c M-." . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines)
  )

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

(provide 'custom-editing)
