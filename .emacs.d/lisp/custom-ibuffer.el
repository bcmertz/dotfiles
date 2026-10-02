;;; custom-ibuffer.el --- ibuffer config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; ibuffer configuration
;;;
;;; Code:

;; better C-x C-b
(use-package ibuffer
  :defer t
  :bind (("C-x C-b" . ibuffer))
  :config
  (bind-key "q" 'kill-current-buffer 'ibuffer-mode-map)
  (defalias 'list-buffers 'ibuffer))

;; group buffers by project root
;; source: prot
(use-package ibuffer-vc
  :after ibuffer
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind (:map ibuffer-mode-map
              ("/ v" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <backspace>" . ibuffer-clear-filter-groups)))

(provide 'custom-ibuffer)
;;; custom-ibuffer.el ends here
