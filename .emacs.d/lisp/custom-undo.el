;;; custom-undo.el --- custom undo -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; use builtin undo commands, and vundo as overlay on top of them
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; highlight momentarily when something is about to be deleted with undo
(use-package undo-hl
  :straight (undo-hl :type git
                     :host github
                     :repo "casouri/undo-hl")
  :hook ((prog-mode . undo-hl-mode)
         (text-mode . undo-hl-mode))
  :config
  (setq undo-hl-flash-duration 0.03)
  :custom-face
  (undo-hl-delete ((t (:inherit region)))))

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory "~/.emacs.d/undo")
  (setq undo-fu-session-ignore-encrypted-files t)
  (setq undo-fu-session-incompatible-major-modes '(magit))
  (setq undo-fu-session-incompatible-files
        (list (concat "\\`" temporary-file-directory)
              "\\`/tmp/"
              "/COMMIT_EDITMSG\\'"
              "\\`/dev/shm/"))
  )


;;;;;;;;;;;;;;;;;;;;;;; Undo Tree - buggy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; undo tree has a nice visualizer and persistent undo history between
;; sessions, but can corrupt undo history for some unfixable reason. It can
;; show diffs though which is nice. Also it overwrites default emacs behavior

;; (defun custom-undo ()
;;   "Custom undo."
;;   (interactive "")
;;   (undo-tree-undo)
;;   (undo-list-transfer-to-tree)
;;   (setq infoo buffer-undo-tree)
;;   (setq temp-buf (get-buffer-create "*tmpy*"))
;;   (save-selected-window
;;     (set-buffer temp-buf)
;;     (switch-to-buffer-other-window temp-buf)
;;     (shrink-window ( / (window-body-height) 2))
;;     (help-mode)
;;     (undo-tree-draw-tree infoo)
;;     (undo-tree-clear-visualizer-data infoo) ;; prevent undo-tree save data corruption
;;     (run-with-local-idle-timer 0.5 t 'kill-buffer-and-its-windows temp-buf)
;;     )
;;   )

;; (defun custom-redo ()
;;   "Custom redo."
;;   (interactive "")
;;   (undo-tree-redo)
;;   (undo-list-transfer-to-tree)
;;   (setq infoo buffer-undo-tree)
;;   (setq temp-buf (get-buffer-create "*tmpy*"))
;;   (save-selected-window
;;     (set-buffer temp-buf)
;;     (switch-to-buffer-other-window temp-buf)
;;     (shrink-window ( / (window-body-height) 2))
;;     (help-mode)
;;     (undo-tree-draw-tree infoo)
;;     (undo-tree-clear-visualizer-data infoo) ;; prevent undo-tree save data corruption
;;     (run-with-local-idle-timer 0.5 t 'kill-buffer-and-its-windows temp-buf)
;;     )
;;   )

;; ;; ;; idk could be nice but isn't really necessary / might not work
;; ;; (defun undo-tree-temp-buffer-visualizer (buffer)
;; ;;   "Put BUFFER something here."
;; ;;   (switch-to-buffer-other-window temp-buf)
;; ;;   (shrink-window ( / (window-body-height) 2))
;; ;;   (help-mode)

;; ;;   ;; (display-buffer-pop-up-window buffer alist)
;; ;;   )

;; ;; ;;
;; ;; (add-to-list 'display-buffer-alist
;; ;;              '("*tmpy*" undo-tree-temp-buffer-visualizer))


;; ;; Undo tree
;; (use-package undo-tree
;;   :defer t
;;   :bind
;;   ("C-_" . text-scale-decrease)
;;   ("C-+" . text-scale-increase)
;;   ("C-M-+" . global-text-scale-increase)
;;   ("C-M-_" . global-text-scale-decrease)
;;   :init
;;   (global-undo-tree-mode)
;;   :config
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
;;   ;; (undo-tree-enable-undo-in-region t)
;;   ;; (undo-tree-visualizer-diff t)
;;   ;; (undo-tree-visualizer-timestamps t)
;;   )

;; ;; (defun set-custom-undo ()
;; ;;   "Set custom undo keys to show tmp undo tree."
;; ;;   (global-set-key [remap undo-tree-undo] 'custom-undo)
;; ;;   (global-set-key [remap undo-tree-redo] 'custom-redo))
;; ;; (apply-if-gui 'set-custom-undo)

;; (defun fix-undo-trickery ()
;;   "Fix undo trickery."
;;   ;; doesn't seem necessary but is probably good to remember these patterns
;;   ;; (add-hook 'undo-tree-mode-hook
;;   ;;           (lambda () (define-key undo-tree-map (kbd "C-_") 'text-scale-decrease)))
;;   ;; (global-set-key (kbd "C-_") 'text-scale-decrease)
;;   (with-eval-after-load "undo-tree"
;;     (define-key undo-tree-map (kbd "C-_") 'text-scale-decrease)
;;     (define-key undo-tree-map (kbd "C-M-_") 'global-text-scale-decrease)))
;; (apply-if-gui 'fix-undo-trickery)


;; ;; prevent undo-tree from messaging when it's saving the undo-tree history
;; (defun my-undo-tree-save-history (undo-tree-save-history &rest args)
;;   "Prevent undo-tree from messaging save alerts given UNDO-TREE-SAVE-HISTORY ARGS."
;;   (let ((message-log-max nil)
;;         (inhibit-message t))
;;     (apply undo-tree-save-history args)))

;; (advice-add 'undo-tree-save-history :around 'my-undo-tree-save-history)

(provide 'custom-undo)
;;; custom-undo.el ends here
