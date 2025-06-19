;;; custom-fonts.el --- custom fonts -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; Code:

;; Default font (cant be font with hyphen in the name like current default monospace Inconsolata-g)
(setq initial-frame-alist '((font . "Monospace")))
(setq default-frame-alist '((font . "Monospace")))

;; (add-to-list 'default-frame-alist '(font-backend . "ftcrhb")) ; default i think... https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html

;; cool fonts to try - ETBembo, Source Sans Pro, Lucida Grande, Verdana
;; Emoji: 😄, 🤦, 🏴, ,  ;; should render as 3 color emojis and 2 glyphs
(defun styling/set-fonts()
  "Set the emoji and glyph fonts."
  (set-fontset-font "fontset-default" '(#xE0B4 . #xE0B6)
                    "UbuntuMono Nerd Font" nil 'prepend)

  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'prepend)
  (set-fontset-font t 'symbol "Siji" nil 'prepend)
  (set-fontset-font t 'symbol "UbuntuMono Nerd Font" nil 'prepend)

  (set-face-attribute 'default nil :font "Monospace" :weight 'light :height 130)
  (set-face-attribute 'fixed-pitch nil :font "Monospace" :weight 'light :height 130)
  (set-face-attribute 'variable-pitch nil :font "ETBembo" :weight 'thin :height 118)
  )

;; respect default terminal fonts
;; if we're in a gui set the fonts appropriately
;; for daemon sessions and and nondaemons
(add-hook 'emacs-startup-hook
          (lambda ()
            (apply-if-gui 'styling/set-fonts)))

(defun set-buffer-font ()
  "Set buffer font."
  (interactive "")
  (let* ((completion-ignore-case t)
          (default (frame-parameter nil 'font))
	  (font (completing-read (format-prompt "Font name" default)
				 ;; x-list-fonts will fail with an error
				 ;; if this frame doesn't support fonts.
				 (font-family-list  (selected-frame))
                                 nil nil nil nil default)))
    (when (or (stringp font) (fontp font))
      (setq buffer-face-mode-face (list ':family font))
      (buffer-face-mode))))

(defun unset-buffer-font ()
  "Unset buffer font."
  (interactive "")
  (setq buffer-face-mode-face nil)
  (buffer-face-mode -1))

;; switch font
(global-set-key (kbd "C-c t f") 'set-buffer-font)
(global-set-key (kbd "C-c t F") 'unset-buffer-font)

(use-package nerd-icons
  :defer t
  ;:custom
  ;(nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (if (not (file-exists-p "~/.local/share/fonts/NFM.ttf"))  (nerd-icons-install-fonts))
  )

;; other interesting icon packages to consider
;; https://github.com/rainstormstudio/nerd-icons-completion
;; https://github.com/LuigiPiucco/nerd-icons-corfu

(provide 'custom-fonts)
;;; custom-fonts.el ends here
