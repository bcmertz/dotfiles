;;; custom-windowing.el --- window manager
;;;
;;; Commentary:
;;;
;;; moving and resizing windows / buffers
;;;
;;; Code:
;; emacs window management
(use-package windmove
  :config
  ;; wrap around at edges
  ;; (setq windmove-wrap-around t)
  (windmove-default-keybindings 'meta))             ;; M-arrows to move

(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M--") 'shrink-window-horizontally)

(global-set-key (kbd "<M-S-up>")     'buf-move-up)
(global-set-key (kbd "<M-S-down>")   'buf-move-down)
(global-set-key (kbd "<M-S-left>")   'buf-move-left)
(global-set-key (kbd "<M-S-right>")  'buf-move-right)

(global-set-key (kbd "C-x |") 'toggle-window-split)

;;; custom-windowing.el ends here
