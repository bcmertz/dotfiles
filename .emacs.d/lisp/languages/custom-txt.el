;;; custom-txt.el --- plain text configuration
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package text-mode
  :mode "\\.txt\\'"
  :defer t
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              ;; better flyspell interface that doesn't enable everywhere
              ;; but even this has the annoying TAB spellchecking binding
              (wucuo-start)
              ;; C-e goes to the end of the visual line not the logical line
              (turn-on-visual-line-mode)
              ;; dont have super long lines, break them
              (setq word-wrap t)
              )
            )
  )

;; TODO fix this pls
;; check spelling on the fly
;; this isn't working for some reason and is enabling itself in every file type including text files
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))

;; should these be ispell commnads?
(global-set-key (kbd "C-i") 'ispell-word)
(global-set-key (kbd "M-i") 'ispell-buffer)


(use-package define-word
  :defer t)


;;; custom-txt.el ends here
