;;; custom-selection.el --- custom selection -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; move point (cursor) to mark (start of selection)
(global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)

;; make shift click select region
(mouse-shift-adjust-mode)

;; replace highlighted sections
;; by default emacs will not delete selection text when typing on it
(delete-selection-mode t)

(defun jump-to-region-beginning ()
  "Move cursor to end of active region if there is one."
  (interactive "")
  (if (use-region-p)
      (progn
        (goto-char (region-beginning))
        (keyboard-quit))))

(defun jump-to-region-end ()
  "Move cursor to end of active region if there is one."
  (interactive "")
  (if (use-region-p)
      (progn
        (goto-char (region-end))
        (keyboard-quit))))

;; select current line
(defun mark-entire-line ()
  "Mark the whole line from the indent to the end."
  (interactive)
  (beginning-of-line-text)
  (set-mark-command nil)
  (end-of-line))

(global-set-key (kbd "C-x C-l") 'mark-entire-line)

;; Multiple Cursors
(use-package multiple-cursors
  :defer t
  :bind*
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("M-/" . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines))

(use-package multiple-cursors-core
  :straight multiple-cursors
  :defer t
  :bind (:map mc/keymap
              ("<return>" . nil)
              ("C-<return>" . multiple-cursors-mode)))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


(advice-add 'er/prepare-for-more-expansions-internal :override #'er-custom/prepare-for-more-expansions-internal)

(defun er-custom/prepare-for-more-expansions-internal (repeat-key-str)
  "CUSTOM Return bindings and a message to inform user about them given REPEAT-KEY-STR."
  (let ((msg (format "Type %s to expand again" repeat-key-str))
        (bindings (list (cons repeat-key-str '(er/expand-region 1)))))
    ;; If contract and expand are on the same binding, ignore contract
    (unless (string-equal repeat-key-str expand-region-contract-fast-key)
      (setq msg (concat msg (format ", %s to contract" expand-region-contract-fast-key)))
      (push (cons expand-region-contract-fast-key '(er/contract-region 1)) bindings))
    ;; If reset and either expand or contract are on the same binding, ignore reset
    (unless (or (string-equal repeat-key-str expand-region-reset-fast-key)
                (string-equal expand-region-contract-fast-key expand-region-reset-fast-key))
      (setq msg (concat msg (format ", %s to reset" expand-region-reset-fast-key)))
      (push (cons expand-region-reset-fast-key '(er/expand-region 0)) bindings))
    (setq msg (concat msg (format ", > to jump end" expand-region-eor-fast-key)))
    (push (cons expand-region-eor-fast-key '(jump-to-region-end)) bindings)
    (setq msg (concat msg (format ", < to jump beginning" expand-region-bor-fast-key)))
    (push (cons expand-region-bor-fast-key '(jump-to-region-beginning)) bindings)
    (cons msg bindings)))

(advice-add 'er/expand-region :override #'er-custom/expand-region)

(defun er-custom/expand-region (arg)
  "Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time ARG."
  (interactive "p")
  (if (< arg 1)
      (er/contract-region (- arg))
    (er--prepare-expanding)
    (while (>= arg 1)
      (setq arg (- arg 1))
      (when (eq 'early-exit (er--expand-region-1))
        (setq arg 0)))
    (when (and expand-region-fast-keys-enabled
               (not (memq last-command '(er/expand-region er/contract-region jump-to-region-end jump-to-region-beginning))))
      (er/prepare-for-more-expansions))))

(defcustom expand-region-eor-fast-key "."
  "Key to use after an initial expand/contract to go to end of region."
  :group 'expand-region
  :type 'string)

(defcustom expand-region-bor-fast-key ","
  "Key to use after an initial expand/contract to go to end of region."
  :group 'expand-region
  :type 'string)

(use-package expand-region
  :defer t
  :bind* ("C-;" . er/expand-region))

(provide 'custom-selection)
;;; custom-selection.el ends here
