;;; custom-latex.el --- latex configuratiob
;;;
;;; Commentary:
;;;
;;;
;;; Code:

;; (use-package doc-view
;;   :defer t
;;   :bind
;;   ("<left>" . image-backward-hscroll)
;;   ("<right>" . image-forward-hscroll)
;;   )

(use-package pdf-tools
  :ensure t
  :init ()
  :config (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

;;; custom-latex.el ends here
