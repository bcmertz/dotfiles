;;; custom-styling.el --- custom styling -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; UI, highlighting
;;;
;;; Code:

;; cursor behavior
(setq cursor-type 'box)
(setq visible-cursor t)
(blink-cursor-mode 1)

;; true transparency
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql (cond ((numberp alpha-background) alpha-background)
                    ((numberp (cdr alpha-background)) (cdr alpha-background))
                    )
              100)
         '96 '100))))

(global-set-key (kbd "C-c t r") 'toggle-transparency)

;; Hide line numbering
(global-display-line-numbers-mode -1)

;; margins
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;; remove fringes
;; (set-window-fringes neo-global--window 0 0)
;; (set-display-table-slot standard-display-table 0 ?\ )
;; (set-face-attribute 'fringe nil :background nil)
;; (fringe-mode 0)

;; text sizing commands
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'global-text-scale-increase)
(global-set-key (kbd "C-M-_") 'global-text-scale-decrease)

;; [DEPRECATED]
;; (if-gui (add-hook 'after-init-hook (lambda ()
;;                                      (global-unset-key (kbd "C-_"))
;;                                      (global-set-key (kbd "C-+") 'text-scale-increase)
;;                                      (global-set-key (kbd "C-_") 'text-scale-decrease)
;;                                      (global-set-key (kbd "C-M-+") 'global-text-scale-increase)
;;                                      (global-set-key (kbd "C-M-_") 'global-text-scale-decrease))))


;; center 1 buffer
(use-package centered-window
  :config
  (setq cwm-centered-window-width 130)
  (centered-window-mode t))

;; (use-package spacious-padding
;;   :straight (spacious-padding :type git
;;                               :host github
;;                               :repo "protesilaos/spacious-padding")
;;   ;; :config
;;   ;; (setq spacious-padding-widths '(:internal-border-width 0 :right-divider-width 40 :scroll-bar-width 0))
;;   )
;; (if-gui (spacious-padding-mode 1))

;; remove ugly change in bg color in fringes
;; (set-face-attribute 'fringe nil :background nil)

(defun hide-ui-elements ()
  "Hide Scroll bar,menu bar, tool bar."
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))
(apply-gui-and-term 'hide-ui-elements)

;; toggle tool bar mode [builtin]
(global-set-key (kbd "C-c t s") 'toggle-scroll-bar)

;; toggle tool bar mode
(defun toggle-tool-bar ()
  "Toggle `tool-bar-mode'."
  (interactive)
  (if (eq tool-bar-mode t)
      (tool-bar-mode -1)
    (tool-bar-mode 1))
  )
(global-set-key (kbd "C-c t l") 'toggle-tool-bar)

;; toggle menu bar mode
(defun toggle-menu-bar ()
  "Toggle `menu-bar-mode'."
  (interactive)
  (if (eq menu-bar-mode t)
      (menu-bar-mode -1)
    (menu-bar-mode 1))
  )
(global-set-key (kbd "C-c t m") 'toggle-menu-bar)

;; Highlight current line in gui emacs
(if-gui (global-hl-line-mode 1))

;; toggle hl line mode globally
(defun toggle-hl-line ()
  "Toggle global hl line mode."
  (interactive)
  (if (eq global-hl-line-mode t)
      (global-hl-line-mode -1)
    (global-hl-line-mode 1))
  )

;; toggle hl line mode
(global-set-key (kbd "C-c t h") 'toggle-hl-line)


;; describe char face at mouse click
;; from https://emacs.stackexchange.com/a/35449
;; based on: https://emacs.stackexchange.com/a/19585/13444
;;
;; unable to describe-char in modeline, fringe, etc because describe-char
;; only works in accessible buffers unfortunately. Can get the text though,
;; see https://emacs.stackexchange.com/a/311
(defun my-describe-char-at-mouse-click (click-event)
  "`describe-char' at CLICK-EVENT's position.
CLICK-EVENT should be a mouse-click event."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((pos (cadr (event-start click-event))))
    (describe-char pos)))

(global-set-key (kbd "C-h <down-mouse-1>")
                #'my-describe-char-at-mouse-click)


(provide 'custom-styling)
;;; custom-styling.el ends here
