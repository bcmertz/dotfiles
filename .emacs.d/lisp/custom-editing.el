;;; custom-editing.el --- configuration pertaining to editing text -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; code folding, syntax based packages, multiple cursors, etc
;;;
;;; Code:

(use-package reopen-as-root
  :after tramp
  :straight (reopen-as-root :local-repo "~/.emacs.d/lisp/lib/reopen-as-root"
                            :files ("reopen-as-root.el")
                            :type nil)
  :init (reopen-as-root-mode))

;; code navigation
(use-package avy
  :bind (("M-g" . avy-goto-char)    ;; go to char
         ("M-l" . avy-goto-line)))  ;; go to line

;; zap to char using avy
(use-package avy-zap :defer t)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

;; run comment-line instead of comment-dwim
;; http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
;; I switched this out because comment-dwim breaks lines instead of commenting each line
;; in a region like comment-line does
;; also get rid of annoying web mode binding
(add-hook 'web-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-;"))))
(global-set-key (kbd "M-;") 'comment-line)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; untabify on save
;; (add-hook 'before-save-hook '(lambda()
;;                                (untabify (point-min) (point-max))))

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda()
                               (when (not (or (derived-mode-p 'markdown-mode 'org-mode)))
                                 (delete-trailing-whitespace))))

;; replace highlighted sections
(delete-selection-mode t)		; By default emacs will not delete selection text when typing on it, let's fix it
(setq kill-whole-line t) 			; kills the entire line plus the newline whenever you invoke kill-line (i.e. via C-k).

;; smart parenthesis
(use-package smartparens
  :defer t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (add-hook 'org-mode-hook (lambda () (setq-local smartparens-global-mode nil)))

  (show-smartparens-global-mode +1)
  (add-hook 'org-mode-hook (lambda () (setq-local show-smartparens-global-mode nil)))

  (which-key-add-key-based-replacements "C-c s" "smartparens")
  (global-set-key (kbd "C-c s e") 'sp-show-enclosing-pair)
  (global-set-key (kbd "C-c s u") 'sp-up-sexp)
  (global-set-key (kbd "C-c s d") 'sp-down-sexp)
  (global-set-key (kbd "C-c s i") 'sp-change-enclosing)
  (global-set-key (kbd "C-c s c") 'sp-rewrap-sexp))

;; maybe use puni and electric pair mode in the future in
;; place of smartparens since puni has a lot more knowledge
;; of builtin sexp parsing
;; idk or wait for treesitter enabled libraries to do magic
;;
;; Use puni-mode globally and disable it for term-mode.
;; (use-package puni
;;   :defer t
;;   :init
;;   ;; The autoloads of Puni are set up so you can enable `puni-mode` or
;;   ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
;;   ;; any key that calls Puni commands, it's loaded.
;;   (puni-global-mode)
;;   (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; (use-package electric-pair-mode
;;   :defer t
;;   :init
;;   (electric-pair-mode t)
;;   )

(defun set-sp-face ()
  "Customize matching sp face."
  (add-hook 'smartparens-mode-hook
	    (lambda ()
              (set-face-attribute 'sp-show-pair-match-face nil
                                  :foreground "green"
                                  :background 'unspecified
                                  :weight 'normal
                                  :underline nil ;; "#16A085"
                                  ))))
(apply-if-gui 'set-sp-face)

;; From smartparens documentation
(sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;;;;;;;;;;;;;; code folding ;;;;;;;;;;;;;;;;;;;;;;

;; (use-package vimish-fold
;;   :defer t
;;   :init
;;   (vimish-fold-global-mode 1)
;;   :config
;;   (which-key-add-key-based-replacements "C-c f" "vimish fold")
;;   :bind
;;   ("C-c f f" . vimish-fold)
;;   ("C-c f d" . vimish-fold-delete)
;;   ("C-c f l" . vimish-fold-avy) ;; fold to line
;;   ("C-c f t" . vimish-fold-toggle) ;; fold to line
;;   ("M-`" . vimish-fold-delete-all)
;;   )

;; builtin code folding
;; https://www.emacswiki.org/emacs/HideShow
(use-package hs-minor-mode
  :straight (:type built-in)
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (which-key-add-key-based-replacements "C-c f" "folding")
  (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
  :config
  (setq hs-hide-comments nil)
  :bind
  ("C-c f f" . hs-hide-block)
  ("C-c f t" . hs-toggle-hiding)
  ("C-c f d" . hs-show-block)
  ("C-c f a" . hs-show-all)
  ("M-`" . hs-show-all)
  (:map hs-minor-mode-map
        ("C-c @" . nil))
  )


;;;;;;;;;;;;;;;; text selection ;;;;;;;;;;;;;;;

(defun jump-to-region-beginning ()
  "Move cursor to end of active region if there is one."
  (interactive "")
  (if (use-region-p)
      (progn
        (goto-char (region-beginning))
        (keyboard-quit))
    ))

(defun jump-to-region-end ()
  "Move cursor to end of active region if there is one."
  (interactive "")
  (if (use-region-p)
      (progn
        (goto-char (region-end))
        (keyboard-quit))
    ))

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
  :bind*("C-;" . er/expand-region)
  )


;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-e") 'eval-buffer)

(global-set-key (kbd "C-x C-l") 'mark-entire-line)

(global-set-key (kbd "<M-RET>") 'return-newline-below)     ;; return new line below

;; use 'transpose-regions for region marking
(global-set-key [(control up)] 'move-lines-up)
(global-set-key [(control down)] 'move-lines-down)

;; xref navigate definitions
(use-package xref
  :straight (:type built-in)
  :bind*
  ("C-." . xref-find-definitions)
  ("C-M-." . xref-find-definitions-other-window)
  ("C-," . xref-go-back)
  ("C-M-," . xref-go-forward))

;; Multiple Cursors
(use-package multiple-cursors
  :defer t
  :bind*
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("C-c M-." . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines))

(use-package multiple-cursors-core
  :straight nil
  :defer t
  :bind (:map mc/keymap
              ("<return>" . nil)
              ("C-<return>" . multiple-cursors-mode)))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key [C-backspace] 'backward-delete-word)
(global-set-key [C-delete] 'forward-delete-word)

;; rename file and buffer
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)


;; builtin autoreverting
(use-package autorevert
  :straight (:type built-in)
  :config
  (setq auto-revert-interval 0.25)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose t)
  (global-auto-revert-mode +1))

(provide 'custom-editing)
;;; custom-editing.el ends here
