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
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("q" . kill-current-buffer))
  :config
  (setq ibuffer-human-readable-size t)
  (defalias 'list-buffers 'ibuffer))

;; group buffers by project root
;; source: prot
(use-package ibuffer-vc
  :after ibuffer
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root)
  :bind (:map ibuffer-mode-map
              ("/ v" . ibuffer-vc-set-filter-groups-by-vc-root)
              ("/ <backspace>" . ibuffer-clear-filter-groups))
  :config
  (setq ibuffer-saved-filter-groups
        '(("Main"
           ("Programming" (or (mode . css-mode)
                              (mode . emacs-lisp-mode)
                              (mode . html-mode)
                              (mode . mhtml-mode)
                              (mode . python-mode)
                              (mode . scss-mode)
                              (mode . shell-script-mode)
                              (mode . yaml-mode)))
           ("Markdown" (mode . markdown-mode))
           ("Magit" (or (mode . magit-blame-mode)
                        (mode . magit-cherry-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-log-mode)
                        (mode . magit-process-mode)
                        (mode . magit-status-mode)))
           ("Special" (or (name . "\\`\\*Help\\*\\'")
                          (name . "\\`\\*Custom.*")
                          (name . "\\`\\*Org Agenda\\*\\'")
                          (name . "\\`\\*straight.*\\'")
                          (name . "\\`\\*eldoc.*\\'")
                          (name . "\\`\\*info\\*\\'")
                          (name . "\\`\\*scratch.*\\*\\'")
                          (name . "\\`\\*Backtrace\\*\\'")
                          (name . "\\`\\*Messages\\*\\'")
                          (name . "\\`\\*Warnings\\*\\'")))
           ("Terminals" (or (name . "\\`\\*.*ghostel.*\\'")))
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode)))))
  )

(provide 'custom-ibuffer)
;;; custom-ibuffer.el ends here
