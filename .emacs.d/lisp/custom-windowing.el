;;; custom-windowing.el --- window manager -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; tabs, moving and resizing windows / buffers
;;;
;;; Code:

;; better C-x C-b
(use-package ibuffer
  :defer t
  :bind (("C-x C-b" . ibuffer))
  :config
  (bind-key "q" 'kill-current-buffer 'ibuffer-mode-map)
  (defalias 'list-buffers 'ibuffer))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode -1) ;; only use centaur-tab-history-mode
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  ;;  (setq centaur-tabs-set-bar 'left)
  :bind
  ("C-M-<left>" . centaur-tabs-backward)
  ("C-M-<right>" . centaur-tabs-forward)
  )

(defun toggle-centaur-tabs ()
  "Toggle centaur tabs mode."
  (interactive)
  (if (eq centaur-tabs-mode t)
      (centaur-tabs-mode -1)
    (centaur-tabs-mode 1))
  )

;; toggle hl line mode
(global-set-key (kbd "C-c t b") 'toggle-centaur-tabs)

;; https://config.daviwil.com/emacs#control-buffer-placement
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))
;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)


(use-package windmove
  :after org
  :config
  ;; wrap around at edges
  ;; (setq windmove-wrap-around t)
  (windmove-default-keybindings 'meta) ;; M-arrows to move
  )

(use-package buffer-move
  :defer t)

(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

(global-set-key (kbd "M-S-<up>")    'buf-move-up)
(global-set-key (kbd "M-S-<down>")  'buf-move-down)
(global-set-key (kbd "M-S-<left>")  'buf-move-left)
(global-set-key (kbd "M-S-<right>") 'buf-move-right)

(global-set-key (kbd "C-M-=") 'enlarge-window)
(global-set-key (kbd "M-=")   'enlarge-window-horizontally)
(global-set-key (kbd "C-M--") 'shrink-window)
(global-set-key (kbd "M--")   'shrink-window-horizontally)

(global-set-key (kbd "C-x |") 'toggle-window-split)
(global-set-key (kbd "C-x w d") 'toggle-window-dedicated)

;; from https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun parent-split-below (root)
  "Split window right from the parent or from root with ROOT."
  (interactive "P")
  (split-window (if root (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

(defun parent-split-right (root)
  "Split window below from the parent or from root with ROOT."
  (interactive "P")
  (split-window (if root (frame-root-window)
                  (window-parent (selected-window)))
                nil 'right nil))

;; use C-u C-x @/# for ROOT argument
(global-set-key (kbd "C-x @") 'parent-split-below)
(global-set-key (kbd "C-x #") 'parent-split-right)

;; set dedicated side slots for left top right bottom
;; neotree | nil | help, embark, etc | terminal
(setq window-sides-slots '(1 0 2 1))

;; allow switching to buffer in strongly dedicated windows.
;; pop - perform pop-to-buffer instead
(setq switch-to-buffer-in-dedicated-window 'pop)

;; If non-nil, switch-to-buffer runs pop-to-buffer-same-window instead.
(setq switch-to-buffer-obey-display-actions t)

;;;;;;;;;;;;;;;;; window rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'display-buffer-alist
             '((or (major-mode . Info-mode)
                   (major-mode . help-mode)
                   (major-mode . helpful-mode))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . right)
               (slot . -1)
               (window-width . 0.4)))

(add-to-list 'display-buffer-alist
             '((or (major-mode . embark-collect-mode)
                   (major-mode . grep-mode))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . right)
               (slot . 1)
               (window-width . 0.4)))

(add-to-list 'display-buffer-alist
             '((major-mode . magit-status-mode)
               (display-buffer-full-frame)))

;; control where vterm appears - place it into dedicated bottom slot 1/4 of screen
(add-to-list 'display-buffer-alist
             '((major-mode . vterm-mode)
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (slot . 0)
               (side . bottom)
               (reusable-frames . visible)))


;; (use-package popper
;;   :defer t
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :config
;;   (setq popper-group-function #'popper-group-by-project) ;; projects
;;   ;; (setq popper-group-function #'popper-group-by-projectile) ;; projectile projects
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))                ; For echo area hints

;; EXAMPLE
;; (defun adjust-window-split-thresholds nil
;;   "Adjust split thresholds so that popup windows always split vertically in a tall frame, horizontally in a wide frame, with a maximum of two columns"
;;   (interactive)
;;   (if (>= (frame-pixel-width) (frame-pixel-height))
;;       ; wide frame
;;       (progn
;;         (setq split-height-threshold (frame-height))
;;         (setq split-width-threshold  (/ (frame-width) 2))
;;         )
;;       ; tall frame
;;       (progn
;;         (setq split-height-threshold (frame-height))
;;         (setq split-width-threshold  (frame-width)))))
;; (add-hook 'window-configuration-change-hook 'adjust-window-split-thresholds)


(provide 'custom-windowing)
;;; custom-windowing.el ends here
