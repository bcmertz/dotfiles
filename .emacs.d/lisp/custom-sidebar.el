;;; custom-sidebar.el --- sidebar config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; neotree sidebar config
;;;
;;; Code:

;; Sidebar File navigation
(use-package neotree ;; C-c C-c makes the focused directory the new root view
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
  :bind (("C-\\" . neotree-project-dir-toggle)
         :map neotree-mode-map (("<up>" . neotree-previous-line)
                                ("<down>" . neotree-next-line)))
  :custom-face
  (neo-dir-link-face  ((t (:inherit fixed-pitch))))
  (neo-header-face    ((t (:inherit fixed-pitch))))
  (neo-banner-face    ((t (:inherit fixed-pitch))))
  (neo-root-dir-face  ((t (:inherit fixed-pitch))))
  (neo-file-link-face ((t (:inherit fixed-pitch))))
  )

(defun set-neotree-settings ()
  "Function to set neotree settings buffer locally."
  (add-hook 'neotree-mode-hook
	    (lambda ()
              ;; remove margins
              (setq-local left-margin-width 0 right-margin-width 0)
              (set-window-buffer nil (current-buffer))

              ;; no modeline
	      (setq mode-line-format nil)
              (buffer-face-mode 1))))

;; apply if gui our neotree settings
(apply-if-gui 'set-neotree-settings)

(defun set-neotree-styling ()
  "Set neotree styling."
  (face-remap-add-relative 'fringe nil :background (get-theme-variable-from-palette 'bg-alt))
  (buffer-face-set :background (get-theme-variable-from-palette 'bg-alt))
  (buffer-face-mode 1)
  (set-face-attribute 'neo-root-dir-face nil :extend t :box nil :background (get-theme-variable-from-palette 'bg-alt)))

(advice-add 'neotree-show :after #'set-neotree-styling)

;; The cursor always sits at bol. `+neotree--fix-cursor-h' and
;; `+neotree--indent-cursor-a' change that behavior so that the cursor is
;; always on the first non-blank character on the line, in the neo buffer. (src - doom-emacs)
(add-hook! 'neo-enter-hook
  (defun +neotree-fix-cursor-h (&rest _)
    (with-current-buffer neo-global--buffer
      (+neotree--indent-cursor-a))))

(defadvice! +neotree--indent-cursor-a (&rest _)
  :after '(neotree-next-line neotree-previous-line)
  (beginning-of-line)
  (skip-chars-forward " \t\r"))

(provide 'custom-sidebar)
;;; custom-sidebar.el ends here
