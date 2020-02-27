;; Don't gc on startup
(setq gc-cons-threshold most-positive-fixnum)

;; Packages and repository management
(require 'package)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-1" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("melpa-2" . "https://melpa.org/packages/") t)

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Configuration of emacs in all modes
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "general.el")                     ; my configuration
(load "styling.el")                     ; styling config

(setq initial-buffer-choice "~/go/src/github.com/getlantern/todo.org")
(setq default-directory "~/go/src/github.com/getlantern/" )

;;(require 'custom-exwm-config)           ; use exwm instead of default xinitrc
;;(custom-exwm-config)

;; Language specific configuration
(add-to-list 'load-path "~/.emacs.d/languages")
(load "custom-golang.el")
(load "custom-js.el")
(load "custom-markdown.el")
(load "custom-css.el")

;; Return to normal gc value
(setq gc-cons-threshold 800000)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" default))
 '(display-line-numbers nil)
 '(fci-rule-color "#073642")
 '(file-name-shadow-mode nil)
 '(gofmt-command "goimports" t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d6" "#07b9463c4d36" "#47a3341e358a" "#1d873c3f56d5" "#2d86441c3361" "#43b7362d3199" "#061d417f59d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-border "#93a1a1")
 '(magit-diff-use-overlays nil)
 '(markdown-command "/usr/bin/pandoc")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(vimish-fold origami kaolin-themes ripgrep find-file-in-project flx-ido projectile smartparens rainbow-delimiters mic-paren atom-one-dark-theme spaceline smart-mode-line yasnippet all-the-icons atom-dark-theme exwm ace-window try use-package which-key counsel swiper company-go company flymake-go popup-complete evil neotree direx peep-dired ranger avy nlinum-relative nlinum ag xref-js2 js2-refactor js2-mode go-rename magit markdown-mode go-eldoc go-autocomplete auto-complete flycheck go-mode))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(speedbar-show-unknown-files t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(winner-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
