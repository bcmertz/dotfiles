;;; custom-navigation.el --- nav config
;;;
;;; Commentary:
;;;
;;; moving around files and buffers and perspectives and windows in Emacs
;;;
;;; Code:

;; code navigation
(use-package avy
  :ensure t
  :bind (("M-g" . avy-goto-char)    ;; go to char
         ("M-l" . avy-goto-line)))  ;; go to line

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(defun pipe-out-selection ()
  "Return the highlighted string and continue ivy minibuffer completion."
  (interactive)
  )

;; generic completion frontend
(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         (:map ivy-minibuffer-map
               ("C-c C-r" . ivy-resume)
               ("C-c C-o" . ivy-occur) ;; open list in buffer
               ("<C-return>" . pipe-out-selection)
               ("<M-return>" . ivy-immediate-done)) ;; ignore suggestion and return current entry
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; beautify ivy
;; pretty but slows down switching bufffers too much
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev
        ;; ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode 1))

;; slooooow
;; (use-package all-the-icons-ivy-rich
;;   :ensure t
;;   :defer t
;;   :init (all-the-icons-ivy-rich-mode 1)
;;   )

;; project navigation
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/coding/"))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))



;; perspective navigation
(use-package perspective
  :bind (
         ;; ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         ("C-M-<left>" . persp-prev)
         ("C-M-<right>" . persp-next)
         ("C-M-<return>" . persp-switch)
         ("C-M-<delete>" . persp-state-save)
         ("C-M-<backspace>" . persp-state-load)
         )
  :init
  (setq persp-initial-frame-name "main")
  (setq persp-sort 'created)
  (persp-mode)
  :config
  ;; TODO figure out why restoring expects a directory
  (setq persp-state-default-file "~/.emacs.d/save-perspective")
  (add-hook 'kill-emacs-hook #'persp-state-save)
  )

;; https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
;; basically reuse display buffers that have been previously opened
;; break neotree
;; (setq display-buffer-alist
;;       '((".*" (display-buffer-reuse-window display-buffer-same-window))))
;; (setq display-buffer-reuse-frames t)         ; reuse windows in other frames
;; (setq even-window-sizes nil)                 ; display-buffer: avoid resizing
;; stateful window managemnt
(winner-mode 1)

;; Sidebar File navigation
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
(global-set-key (kbd "M-s") 'counsel-ag)    ;; C-c C-o 'ivy-occur "Search All Results"

;; better C-x C-b
(defalias 'list-buffers 'ibuffer)

;; make projectile commands prettier, doesn't effect ivy C-x b
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Ido like M-x command completion
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex)
  )

;;; custom-navigaton.el ends here
