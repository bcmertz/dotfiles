;;; custom-txt.el --- plain text configuration
;;;
;;; Commentary:
;;;
;;; Handle text entry and spell checking
;;;
;;; Code:

(use-package text-mode
  :mode "\\.txt\\'"
  :defer t
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              ;; C-e goes to the end of the visual line not the logical line
              (turn-on-visual-line-mode)

              ;; dont have super long lines, break them
              (setq word-wrap t)
              )
            )
  )

;; spell checking on the fly
(use-package flyspell
  :after (ispell)
  :defer t
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              )
  )


(defun setup-flyspell ()
  "Setup spell checking on the fly."
  ;; turn on flyspell mode in text files and check entire buffer
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda ()
                     (flyspell-mode 1)
                     (flyspell-buffer)
                     )))
  ;; no flyspell in change log and log edit modes
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  ;; only spellcheck comments in program files
  (dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda () (flyspell-prog-mode))))
  )

(apply-if-gui 'setup-flyspell)

(defun set-C-i ()
  "C+i and tab are by default treated as the same, so rebind it to Hyper+i."
  ;; Translate the problematic keys to the function key Hyper:
  (keyboard-translate ?\C-i ?\H-i)
  ;; Rebind then accordingly:
  (global-set-key [?\H-i] 'ispell-word)
  )

;; Map C-i to ispell-word
(apply-if-gui 'set-C-i)
;; check spelling of region
(global-set-key (kbd "M-i") 'ispell-region)

(use-package define-word
  :defer t
  )


;;; custom-txt.el ends here
