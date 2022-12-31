;;; custom-sidebar.el --- sidebar config
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
  :bind ("C-\\" . neotree-project-dir-toggle)
  :custom-face
  (neo-dir-link-face  ((t (:inherit fixed-pitch))))
  (neo-header-face    ((t (:inherit fixed-pitch))))
  (neo-banner-face    ((t (:inherit fixed-pitch))))
  (neo-root-dir-face  ((t (:inherit fixed-pitch))))
  (neo-file-link-face ((t (:inherit fixed-pitch))))
  )

(defun set-neotree-styling ()
  "Function to style neotree buffer locally."
  (add-hook 'neotree-mode-hook
	    (lambda ()
              ;; remove margins
              (setq-local left-margin-width 0 right-margin-width 0)
              (set-window-buffer nil (current-buffer))

              ;; no modeline
	      (setq mode-line-format nil)

              ;; enable variable pitch fonts
	      (buffer-face-mode 1)

	      ;; ;; locally change the buffer background color
	      ;; (setq buffer-face-mode-face `(:background "#21252B"))
	      ;; ;; if we want a fringe set it to a nice color
	      ;; ;; but only do it locally in neotree buffers
	      ;; (face-remap-add-relative 'fringe nil
	      ;;   		       '(:background "#21252B"))
	      ;; ;; title on neotree
	      ;; (set-face-attribute 'neo-root-dir-face nil
	      ;;   	          :box nil
	      ;;   	          ;; (:line-width 4 :color #21252B) doesn't work for some reason
	      ;;   	          :background "#21252B")

              ;; hl line background locally
	      ;; (face-remap-add-relative 'hl-line nil
	      ;;   	          :background "#353645") ;; #353645 gray ;; #4C77CB blue
              )))

;; apply if gui our neotree styling
(apply-if-gui 'set-neotree-styling)

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

;;; custom-sidebar.el ends here
