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
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
;;  (setq centaur-tabs-set-bar 'left)
  :bind
  ("C-M-<left>" . centaur-tabs-backward)
  ("C-M-<right>" . centaur-tabs-forward))

(defun toggle-centaur-tabs ()
  "Toggle centaur tabs mode."
  (interactive)
  (if (eq centaur-tabs-mode t)
      (centaur-tabs-mode -1)
    (centaur-tabs-mode 1))
  )

;; toggle hl line mode
(global-set-key (kbd "C-c t b") 'toggle-centaur-tabs)


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


(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)


(global-set-key (kbd "C-M-=") 'enlarge-window)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M--") 'shrink-window)
(global-set-key (kbd "M--") 'shrink-window-horizontally)

(global-set-key (kbd "<M-s-up>")     'buf-move-up)
(global-set-key (kbd "<M-s-down>")   'buf-move-down)
(global-set-key (kbd "<M-s-left>")   'buf-move-left)
(global-set-key (kbd "<M-s-right>")  'buf-move-right)

(global-set-key (kbd "C-x |") 'toggle-window-split)

;;; custom-windowing.el ends here
