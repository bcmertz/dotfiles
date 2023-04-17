;;; custom-windowing.el --- window manager -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; tabs, moving and resizing windows / buffers
;;;
;;; Code:
;; emacs window management

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

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
;;  (setq centaur-tabs-set-bar 'left)
  :bind
  ;; ("C-<tab>" . centaur-tabs-backward)
  ;; ("C-<iso-lefttab>" . centaur-tabs-forward)
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

;; stateful window management
(use-package winner
  :config
  (winner-mode)
  ;; (global-set-key (kbd "C-M-<right>") 'winner-redo)
  ;; (global-set-key (kbd "C-M-<left>") 'winner-undo)
  )

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

(provide 'custom-windowing)
;;; custom-windowing.el ends here
