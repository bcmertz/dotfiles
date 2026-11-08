;;; custom-terminal.el --- terminal config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; terminal
;;;
;;; Code:

(use-package util-funcs
  :straight (util-funcs :type nil :local-repo "~/.emacs.d/lisp/lib/util-funcs/"))

(use-package ghostel
  :defer t
  :straight (ghostel :includes (ghostel-project
                                ghostel-compile))
  :commands (ghostel-buffer-list)
  :bind (("C-<return>" . toggle-terminal)
         :map ghostel-semi-char-mode-map
         ("C-<return>" . toggle-terminal)
         ("C-s"  . consult-line)))

(use-package ghostel-project
  :after ghostel
  :bind (:map project-prefix-map
              ("t" . ghostel-project)
              ("T" . ghostel-project-list-buffers)))

(defun ghostel-buffers-default ()
  "Return list of default non-project ghostel buffers."
  (let* ((bufs (seq-filter (lambda (buf) (string-prefix-p ghostel-buffer-name (buffer-name buf)))
                         (ghostel-buffer-list))))
    bufs))

(defun toggle-terminal ()
  "Toggle ghostel terminal."
  (interactive)
  (let* ((proj (current-project-root))
         (gh-list-cmd (if proj 'ghostel-project-buffer-list 'ghostel-buffers-default))
         (gh-new-cmd (if proj 'ghostel-project-new 'ghostel-new))
         (term-buffer (car (funcall gh-list-cmd))))
    (if term-buffer
        (if-let* ((term-window (get-buffer-window term-buffer)))
            ;; yes term buffer and window
            (quit-window nil term-window)
          ;; yes term buffer no window
          (progn
            (display-buffer term-buffer)
            (select-window (get-buffer-window term-buffer))))
      ;; no term buffer
      (funcall gh-new-cmd))))

;; (use-package vterm
;;   :defer t
;;   :commands vterm--internal
;;   :bind (("C-<return>" . vterm-toggle)
;;          :map vterm-mode-map (("C-c C-t" . vterm-copy-mode))))
;;
;; (defun vterm-toggle ()
;;   "Toggle vterm."
;;   (interactive)
;;   (let ((term-buffer (get-buffer "*vterm*"))
;;         (term-window (get-buffer-window "*vterm*")))
;;     (if term-buffer
;;         ;; if vterm exists
;;         (if term-window
;;             ;; and is visible
;;             (quit-window nil term-window)
;;           ;; and isn't visible
;;           (progn
;;             (display-buffer term-buffer)
;;             (select-window (get-buffer-window "*vterm*"))))
;;       ;; if vterm doesn't exist
;;       (vterm--internal #'display-buffer))))

(provide 'custom-terminal)
;;; custom-terminal.el ends here
