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
;; also get rid of annoying web mode binding
(add-hook 'web-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-;"))))
(global-set-key (kbd "M-;") 'comment-line)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; untabify on save
;; (add-hook 'before-save-hook '(lambda()
;;                                (untabify (point-min) (point-max))))

;; delete trailing whitespace on save
(add-hook 'before-save-hook '(lambda()
                               (when (not (or (derived-mode-p 'markdown-mode)))
                                 (delete-trailing-whitespace))))

;; replace highlighted sections
(delete-selection-mode 1)

;; smart parenthesis
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (add-hook 'org-mode-hook (lambda () (setq-local smartparens-global-mode nil)))

  (show-smartparens-global-mode +1)
  (add-hook 'org-mode-hook (lambda () (setq-local show-smartparens-global-mode nil)))

  (which-key-add-key-based-replacements "C-c s" "smartparens")
  (global-set-key (kbd "C-c s e") 'sp-show-enclosing-pair)
  (global-set-key (kbd "C-c s u") 'sp-up-sexp)
  (global-set-key (kbd "C-c s d") 'sp-down-sexp)
  (global-set-key (kbd "C-c s i") 'sp-change-enclosing)
  (global-set-key (kbd "C-c s c") 'sp-rewrap-sexp)
  )

(defun set-sp-face ()
  "Customize matching sp face."
  (add-hook 'smartparens-mode-hook
	    (lambda ()
              (set-face-attribute 'sp-show-pair-match-face nil
                                  :foreground "green"
                                  :background nil
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

;; code folding
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(which-key-add-key-based-replacements "C-c f" "vimish fold")
(global-set-key (kbd "C-c f f") #'vimish-fold)
(global-set-key (kbd "C-c f d") #'vimish-fold-delete)
(global-set-key (kbd "C-c f l") #'vimish-fold-avy) ;; fold to line
(global-set-key (kbd "C-c f t") #'vimish-fold-toggle) ;; fold to line
(global-set-key (kbd "M-`") #'vimish-fold-delete-all)

;; text selection
(use-package expand-region
  :ensure t
  :bind("C-;" . er/expand-region)
  )

;; Undo tree
(use-package undo-tree
  :ensure t
  :bind
  ;; hack to avoid conflict
  ("C-_" . text-scale-decrease)
  ("C-+" . text-scale-increase)
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
  ;; (setq which-key-paging-prefixes '(""))
  ;; (setq which-key-paging-key "<mouse-5>") ;; scroll down
  ;; (setq which-key-paging-key "<mouse-4>") ;; scroll up
  ;; (setq which-key-popup-type 'side-window)
  ;; (setq which-key-side-window-max-height 0.66)
  (which-key-mode))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; rename file and buffer
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;;; custom-editing.el ends here
