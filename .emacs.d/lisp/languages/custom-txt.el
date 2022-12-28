;;; custom-txt.el --- plain text configuration
;;;
;;; Commentary:
;;;
;;; Handle text entry and spell checking
;;;
;;; Code:

(use-package text-mode
  :straight nil
  :mode "\\.txt\\'"
  :defer t
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              ;; C-e goes to the end of the visual line not the logical line
              (turn-on-visual-line-mode)

              ;; dont have super long lines, break them
              (setq word-wrap t))))


(use-package ispell
  :straight nil
  :config
  (setq ispell-program-name "aspell")
  (which-key-add-key-based-replacements "C-c i" "spell checking")
  :bind (("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i r" . ispell-region)))

(use-package flyspell
  :straight nil
  :bind (("C-c i B" . flyspell-buffer)
         ("C-c i f" . flyspell-mode)))

(defun setup-flyspell ()
  "Setup spell checking on the fly."
  ;; turn on flyspell mode in text files and check entire buffer
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda ()
                     (flyspell-mode 1)
                     (flyspell-buffer))))
  ;; no flyspell in change log and log edit modes
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  ;; only spellcheck comments in program files
  (dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda () (flyspell-prog-mode)))))

(apply-if-gui 'setup-flyspell)

(use-package flyspell-correct
  :after flyspell
  :config
  (which-key-add-key-based-replacements "C-c i w" "Correct word")
  :bind
  ("C-c i w" . flyspell-correct-wrapper)
  ("C-c i b" . flyspell-correct-buffer)
  ("C-c i r" . flyspell-correct-region))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(defun set-C-i ()
  "C+i and tab are by default treated as the same, so rebind it to Hyper+i."
  ;; Translate the problematic keys to the function key Hyper:
  (keyboard-translate ?\C-i ?\H-i)
  ;; Rebind then accordingly:
  (global-set-key [?\H-i] 'ispell-word)
  )

(defun flyspell-correct-buffer ()
  "Interactively (ivy) correct buffer."
  (interactive "")
  (flyspell-correct-move 0 t nil))

(defun flyspell-correct-region ()
  "Interactively (ivy) correct region."
  (interactive "")
  (if (use-region-p)
      (progn
        (my-flyspell-correct-move (region-beginning) t t (region-end))
        (keyboard-quit))))

(defun my-flyspell-correct-move (position &optional forward rapid position-end)
  "Correct the first misspelled word that occurs before POSITION."
  (interactive "d")
  ;; push mark when starting
  (when (or (not (mark t))
            (/= (mark t) (point)))
    (push-mark (point) t))
  (let ((original-pos (point))
        (target-pos (point))
        (hard-move-point)
        (mark-opos))
    (unwind-protect
        (save-excursion
          (let ((incorrect-word-pos))

            ;; narrow the region
            (overlay-recenter (point))

            (let* ((unsorted-overlay-list
                    (if forward
                        (overlays-in (- position 1) (point-max))
                      (overlays-in (point-min) (+ position 1))))
                   (comp (if forward #'< #'>))
                   (overlay-list (sort
                                  unsorted-overlay-list
                                  (lambda (o1 o2)
                                    (funcall comp
                                             (overlay-start o1)
                                             (overlay-start o2)))))
                   (overlay 'dummy-value))
              (while overlay
                (setq overlay (car-safe overlay-list))
                (setq overlay-list (cdr-safe overlay-list))
                (when (and overlay
                           (flyspell-overlay-p overlay))

                  (setq incorrect-word-pos (overlay-start overlay))
                  (let ((scroll (> incorrect-word-pos (window-end))))
                    (goto-char incorrect-word-pos)
                    (when scroll (ignore-errors (recenter))))

                  ;; Point originally was on misspelled word, so we need to restore
                  ;; it. This imitates just calling `flyspell-correct-at-point'. But
                  ;; gives all the perks of `flyspell-correct-move'.
                  ;;
                  ;; But with rapid mode, `hard-move-point' will be set to nil
                  ;; eventually. Which gives more predictable point location in
                  ;; general.
                  (setq hard-move-point
                        (and (>= original-pos (overlay-start overlay))
                             (<= original-pos (overlay-end overlay))))

                  (if (not (> (point) position-end))
                      ;; Correct a word using `flyspell-correct-at-point'.
                      (let ((res (flyspell-correct-at-point)))
                        (when res
                          ;; stop at misspelled word
                          (when (eq (car-safe res) 'stop)
                            (setq target-pos incorrect-word-pos
                                  hard-move-point t
                                  mark-opos t))

                          ;; break from rapid mode
                          (when (or
                                 ;; treat skip as one-time rapid mode enabler
                                 (and (not (eq (car-safe res) 'skip))
                                      (not rapid))
                                 ;; explicit rapid mode disablers
                                 (eq (car-safe res) 'break)
                                 (eq (car-safe res) 'stop))
                            (setq overlay nil))

                          (when (and
                                 ;; don't push mark if there is no change
                                 (not (memq (car-safe res) '(stop break skip)))
                                 (/= (mark t) (point)))
                            ;; `flyspell-correct-at-point' may move point, use
                            ;; original `incorrect-word-pos' instead
                            (push-mark incorrect-word-pos t))))
                    )
                  )))))

      (when hard-move-point
        (when mark-opos
          (push-mark (point) t))
        (goto-char target-pos))
      ;; We pushed the mark when starting, but if the operation is canceled
      ;; without any change that mark is redundant and needs to be cleaned-up.
      (when (= (mark t) (point)) (pop-mark)))))

;; Map C-i to ispell-word
(apply-if-gui 'set-C-i)
;; check spelling of region
(global-set-key (kbd "M-i") 'ispell-region)

(use-package define-word
  :defer t
  )


;;; custom-txt.el ends here
