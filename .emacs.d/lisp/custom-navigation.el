;;; custom-navigation.el --- nav config
;;;
;;; Commentary:
;;;
;;; moving around files and buffers in Emacs
;;;
;;; Code:
;; Navigation
(use-package avy
  :ensure t
  :bind ("M-g" . avy-goto-char-2)    ;; go to char
  ("M-l" . avy-goto-line))           ;; go to line

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  ("C-r" . swiper))

(ivy-mode 1)
(setq ivy-count-format "(%d/%d) ")

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/coding/" "~/go/src/github.com/getlantern/"))

(use-package perspective
  :bind (
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         ("C-M-<left>" . persp-prev)
         ("C-M-<right>" . persp-next)
         ("C-M-<return>" . persp-switch)
         ("C-M-<delete>" . persp-state-save)
         ("C-M-<backspace>" . persp-state-load)
         )
  :init
  (setq persp-initial-frame-name "emacs")
  :config
  (persp-mode)
  (setq persp-state-default-file "~/.emacs.d/save-perspective")
  (add-hook 'kill-emacs-hook #'persp-state-save)
  )

;; Buffer Management
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)              ; Better C-x b

(defalias 'list-buffers 'ibuffer)    ; better C-x C-b

;; Sidebar File Viewer
(use-package neotree ;; C-c C-c makes the focused directory the new root view
  :ensure t
  :config
  ;; Type H to toggle hidden files
  (setq-default neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-autorefresh nil)
  (setq neo-window-fixed-size ())
  :bind("C-\\" . neotree-project-dir-toggle))

;; Better File Searching
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-s") 'counsel-ag)    ;;;;; C-c C-o 'ivy-occur "Search All Results"

;; Ido like M-x command completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  )

;;; custom-navigaton.el ends here
