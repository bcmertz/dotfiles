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
               ("<M-return>" . ivy-immediate-done)) ;; ignore suggestion and return current entry
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
  (ivy-mode)
  )

;; beautify ivy
;; pretty but slows down switching bufffers too much
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev
        ;; ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-virtual-abbreviate 'full)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1)
  )

(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist
      '(
        (counsel-ag          . ivy-display-function-fallback)
        (swiper          . ivy-display-function-fallback)
        ;; (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
        ;; (complete-symbol . ivy-posframe-display-at-point)
        ;; (t               . ivy-display-function-fallback)
        (t               . ivy-posframe-display-at-frame-center)
        ))
;; https://github.com/tumashu/ivy-posframe/issues/105#issuecomment-750370286
(defun my-ivy-posframe-get-size ()
  "Set the ivy-posframe size according to the current frame."
    (let* ((height (or ivy-posframe-height (or ivy-height 10)))
           (min-height (min height (+ 1 (length ivy--old-cands))))
           (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
      (list
       :height height
       :width width
       :min-height min-height
       :min-width width)))
(setq ivy-posframe-size-function 'my-ivy-posframe-get-size)
(ivy-posframe-mode 1)

;; slooooow
;; (use-package all-the-icons-ivy-rich
;;   :ensure t
;;   :defer t
;;   :init (all-the-icons-ivy-rich-mode 1)
;;   )

;; nice to have in the future for cleaning up recent file suggestions
(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (recentf-mode +1))

;; project navigation
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-indexing-method 'hybrid)
  (setq projectile-mode-line-prefix " ")
  (setq projectile-project-search-path '("~/coding/"))
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
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
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  )

;; stateful window management
(winner-mode 1)

;; Sidebar File navigation
(use-package neotree ;; C-c C-c makes the focused directory the new root view
  :ensure t
  :config
  (setq-default neo-show-hidden-files t) ;; Type H to toggle hidden files
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-mode-line-type 'none
        neo-show-updir-line nil
        neo-autorefresh nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-window-width 30
        neo-window-fixed-size ())
  :bind ("C-\\" . neotree-project-dir-toggle)
  :custom-face
  (neo-dir-link-face  ((t (:inherit variable-pitch))))
  (neo-header-face    ((t (:inherit variable-pitch))))
  (neo-banner-face    ((t (:inherit variable-pitch))))
  (neo-root-dir-face  ((t (:inherit variable-pitch))))
  (neo-file-link-face ((t (:inherit variable-pitch)))))

(defun set-neotree-styling ()
  "Function to style neotree buffer locally."
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      ;; locally change the buffer background color
	      (setq buffer-face-mode-face `(:background "#21252B"))
	      ;; if we want a fringe set it to a nice color
	      ;; but only do it locally in neotree buffers
	      (face-remap-add-relative 'fringe nil
				       '(:background "#21252B"))
	      ;; title on neotree
	      (set-face-attribute 'neo-root-dir-face nil
			          :box nil
			          ;; (:line-width 4 :color #21252B) doesn't work for some reason
			          :background "#21252B")
              ;; hl line background locally
	      (face-remap-add-relative 'hl-line nil
			          :background "#353645") ;; #353645 gray ;; #4C77CB blue
	      ;; no modeline
	      (setq mode-line-format nil)
	      (buffer-face-mode 1))))

;; apply if gui our neotree styling
(apply-if-gui 'set-neotree-styling)

;; from doom-emacs
;;
;; The cursor always sits at bol. `+neotree--fix-cursor-h' and
;; `+neotree--indent-cursor-a' change that behavior so that the cursor is
;; always on the first non-blank character on the line, in the neo buffer.
(add-hook! 'neo-enter-hook
           (defun +neotree-fix-cursor-h (&rest _)
             (with-current-buffer neo-global--buffer
               (+neotree--indent-cursor-a))))

(defadvice! +neotree--indent-cursor-a (&rest _)
  :after '(neotree-next-line neotree-previous-line)
  (beginning-of-line)
  (skip-chars-forward " \t\r"))


;; Better File Searching
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-<S-F>") 'counsel-locate)
(global-set-key (kbd "C-x /") 'find-file-root)
(global-set-key (kbd "M-s") 'counsel-ag)    ;; C-c C-o 'ivy-occur "Search All Results"
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h k") 'counsel-descbinds)

(define-key dired-mode-map (kbd "M-s") 'counsel-ag)

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/ /" . dired-narrow)
              ("/ f" . dired-narrow-fuzzy)
              ("/ r" . dired-narrow-regexp)))

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; from centaur-emacs https://github.com/seagle0128/.emacs.d/blob/334d9afaedd67bc10f207ca72d6daff5ac6469cf/lisp/init-dired.el
  ;; BROKEN - work in progress
  (with-no-warnings
    (defun my-all-the-icons-dired--refresh ()
      "Display the icons of files in a dired buffer."
      (all-the-icons-dired--remove-all-overlays)
      ;; NOTE: don't display icons in remote folders or the folder has too many items
      (if (and (not (file-remote-p default-directory))
               ;; (not (string-match-p "sudo\:root\@localhost" (buffer-file-name)))
               (string-match "localhost" (buffer-file-name))
               (<= (count-lines (point-min) (point-max)) 300))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (dired-get-filename 'relative 'noerror)))
                  (when file
                    (let ((icon (if (file-directory-p file)
                                    (all-the-icons-icon-for-dir file
                                                                :face 'all-the-icons-dired-dir-face
                                                                :height 0.9
                                                                :v-adjust all-the-icons-dired-v-adjust)
                                  (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                      (if (member file '("." ".."))
                          (all-the-icons-dired--add-overlay (point) "  \t")
                        (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
              (forward-line 1)))
        (message "Remote folder or too many items.")))
    (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh)))

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map ("<tab>" . dired-subtree-toggle)))


;; better C-x C-b
(defalias 'list-buffers 'ibuffer)

;; make projectile commands prettier, doesn't effect ivy C-x b
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; custom-navigation.el ends here
