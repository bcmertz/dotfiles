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
(add-hook 'before-save-hook (lambda()
                               (when (not (or (derived-mode-p 'markdown-mode 'org-mode)))
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

(defun custom-undo ()
  "Custom undo."
  (interactive "")
  (undo-tree-undo)
  (undo-list-transfer-to-tree)
  (setq infoo buffer-undo-tree)
  (setq temp-buf (get-buffer-create "*tmpy*"))
  (save-selected-window
    (set-buffer temp-buf)
    (switch-to-buffer-other-window temp-buf)
    (shrink-window ( / (window-body-height) 2))
    (help-mode)
    (undo-tree-draw-tree infoo)
    (undo-tree-clear-visualizer-data infoo) ;; prevent undo-tree save data corruption
    (run-with-local-idle-timer 1 t 'kill-buffer-and-its-windows temp-buf)
    )
  )

(defun custom-redo ()
  "Custom redo."
  (interactive "")
  (undo-tree-redo)
  (undo-list-transfer-to-tree)
  (setq infoo buffer-undo-tree)
  (setq temp-buf (get-buffer-create "*tmpy*"))
  (save-selected-window
    (set-buffer temp-buf)
    (switch-to-buffer-other-window temp-buf)
    (shrink-window ( / (window-body-height) 2))
    (help-mode)
    (undo-tree-draw-tree infoo)
    (undo-tree-clear-visualizer-data infoo) ;; prevent undo-tree save data corruption
    (run-with-local-idle-timer 1 t 'kill-buffer-and-its-windows temp-buf)
    )
  )

;; ;; idk could be nice but isn't really necessary / might not work
;; (defun undo-tree-temp-buffer-visualizer (buffer)
;;   "Put BUFFER something here."
;;   (switch-to-buffer-other-window temp-buf)
;;   (shrink-window ( / (window-body-height) 2))
;;   (help-mode)

;;   ;; (display-buffer-pop-up-window buffer alist)
;;   )

;; ;;
;; (add-to-list 'display-buffer-alist
;;              '("*tmpy*" undo-tree-temp-buffer-visualizer))


;; Undo tree
(use-package undo-tree
  :ensure t
  :bind
  ;; hack to avoid conflict
  ("C-_" . text-scale-decrease)
  ("C-+" . text-scale-increase)
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  ;; (undo-tree-visualizer-diff t)
  ;; (undo-tree-visualizer-timestamps t)
  )

(global-set-key [remap undo-tree-undo] 'custom-undo)
(global-set-key [remap undo-tree-redo] 'custom-redo)

;; prevent undo-tree from messaging when it's saving the undo-tree history
(defun my-undo-tree-save-history (undo-tree-save-history &rest args)
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply undo-tree-save-history args)))

(advice-add 'undo-tree-save-history :around 'my-undo-tree-save-history)



;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
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


;; might be useful for autoreverting like tail for system logs
;; (use-package autorevert
;;   :ensure nil
;;   :config
;;   (setq auto-revert-interval 2)
;;   (setq auto-revert-check-vc-info t)
;;   (setq global-auto-revert-non-file-buffers nil)
;;   (setq auto-revert-verbose nil)
;;   (global-auto-revert-mode +1))

;;; custom-editing.el ends here
