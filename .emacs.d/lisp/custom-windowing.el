;;; custom-windowing.el --- window manager
;;;
;;; Commentary:
;;;
;;; tabs, moving and resizing windows / buffers
;;;
;;; Code:
;; emacs window management

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'left)
  :bind
  ("C-M-<left>" . centaur-tabs-backward)
  ("C-M-<right>" . centaur-tabs-forward))


(use-package windmove
  :after org
  :config
  ;; wrap around at edges
  ;; (setq windmove-wrap-around t)
  (windmove-default-keybindings 'meta) ;; M-arrows to move
  :bind
  ((:map org-mode-map
         ("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)

         ("M-S-<up>" . org-metaup)
         ("M-S-<down>" . org-metadown)
         ("M-S-<left>" . org-metaleft)
         ("M-S-<right>" . org-metaright)
         )))


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
