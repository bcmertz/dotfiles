;;; custom-ivy.el --- ivy config
;;;
;;; Commentary:
;;;
;;; moving around files and buffers and perspectives and windows in Emacs
;;;
;;; Code:

;; counsel
(use-package counsel :defer t)

;; code navigation
(use-package avy
  :bind (("M-g" . avy-goto-char)    ;; go to char
         ("M-l" . avy-goto-line)))  ;; go to line

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(defun ivy-occur-no-read-only ()
  "Ignore attempts to make ivy-occur read-only by default."
  (setq-local inhibit-read-only t))
(add-hook 'ivy-occur-mode-hook 'ivy-occur-no-read-only)
(add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-no-read-only)

;; generic completion frontend
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ;; "M-o" . Additional actions when using ivy-posframe
         ;; ("C-x B" . ivy-switch-buffer-other-window)
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
  ;; orderless completion
  ;; (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  ;; (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  (ivy-mode)
  ;; ignore org roam buffers, use C-c n f
  ;; https://org-roam.discourse.group/t/can-buffer-names-match-note-titles/350/13
  (add-to-list 'ivy-ignore-buffers "^[0-9]\\{14\\}.+\\.org$")
  (add-to-list 'ivy-ignore-buffers "EGLOT*")
  (add-to-list 'ivy-ignore-buffers "\*Async-native-compile-log\*")
  (add-to-list 'ivy-ignore-buffers "\*Compile-Log\*")
  (add-to-list 'ivy-ignore-buffers "magit-diff*")
  (add-to-list 'ivy-ignore-buffers "magit-process*")
  (add-to-list 'ivy-ignore-buffers "\*straight-process\*")
  )

;; beautify ivy
;; pretty but slows down switching bufffers too much
(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev
        ;; ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-virtual-abbreviate 'full)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1)
  )

(use-package ivy-posframe
  :defer t
  :init
  (setq ivy-posframe-display-functions-alist
        '(
          ;; (counsel-ag          . ivy-display-function-fallback)
          (swiper          . ivy-display-function-fallback)
          ;; (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
          ;; (complete-symbol . ivy-posframe-display-at-point)
          ;; (t               . ivy-display-function-fallback)
          (t               . ivy-posframe-display-at-frame-center)
          )
        )
  (setq ivy-posframe-size-function 'my-ivy-posframe-get-size)
  (ivy-posframe-mode 1)
  )

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

;; slooooow
;; (use-package all-the-icons-ivy-rich
;;   :defer t
;;   :init (all-the-icons-ivy-rich-mode 1)
;;   )

(defun my-counsel-find-directory (&optional start-dir)
  "Return a directory chosen by the user.
The user is prompted to choose a directory starting with START-DIR."
  (let ((ivy-read-prompt "Choose directory: ")
        (counsel--find-file-predicate #'file-directory-p)
        (default-directory (or start-dir default-directory)))
    (ivy-read
     ivy-read-prompt
     #'read-file-name-internal
     :matcher #'counsel--find-file-matcher)))


;; Better File Searching
;; C-c C-o 'ivy-occur "Search All Results"
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-<S-F>") 'counsel-locate)
(global-set-key (kbd "C-x /") 'find-file-root)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h k") 'describe-key)
(global-set-key (kbd "C-h K") 'counsel-descbinds)
(global-set-key (kbd "M-s") 'restricted-counsel-ag)
(global-set-key (kbd "M-s") #'(lambda () (interactive "")
                                (counsel-ag nil (my-counsel-find-directory) "--ignore elpa/*")
                                ))
(global-set-key [(meta shift s)] #'(lambda () (interactive "")
                                     (counsel-ag nil (my-counsel-find-directory))))


(define-key dired-mode-map (kbd "M-s") 'counsel-ag)

;;; custom-ivy.el ends here
