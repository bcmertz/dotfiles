;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)


(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;;;;;;;;;;;;;;;;;;;;RUN SERVER / CLIENT SETUP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(server-start) taken care of on startup of linux by $ emacs --daemon


;;;;;;;;;;;;;;;;;;; ALL MODES CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(delete-selection-mode 1)                        ; replace highlighted sections
(electric-pair-mode 1)                           ; fill right

(global-set-key (kbd "M-g M-g") 'avy-goto-line)  ; rebind goto line to be vimmy
(setq avy-style 'at-full)

(global-set-key (kbd "M-s") 'occur)              ; keybinding to use occur

; i dont need relative line numbering with avy / cant figure out the default goto with it
;(display-line-numbers-mode)   ; give display numbers
;(setq display-line-numbers-type "relative")
; i think to use this i'd have to bind M-g M-g to a custom function which subtracts line number entered by the current display-line-number line number or smthing like that

; Deprecated line numbering
;(global-nlinum-relative-mode)                 ; add line numbers on side
;(setq nlinum-relative-redisplay-delay 1)      ; make snappy
;(setq nlinum-relative-current-symbol "->")    ; make pretty
;(setq nlinum-format "%d  ")                   ; make spacious 
;(setq nlinum-relative-toggle)

;;;;;;;;;;;;;;;;;;;;;;NEOTREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key (kbd "C-\\") 'neotree-toggle)         ; like atom
; C-c C-c makes the focused directory the new root view

;;;;;;;;;;;;;;;;;;; JS GOODIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setq js2-mode-show-parse-errors t)
(setq js2-mode-show-strict-warnings nil)
(setq js2-strict-missing-semi-warning nil)


;;;;;;;;;;;GOOOOOOO LANGGGGGG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "/place/where/you/put/it/")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

`(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(defun go-mode-setup ()
  ;; use C-c C-j to jump to definition and C-u C-x C-x to jump back
  (go-eldoc-setup) ;; uses gocode and go-mode
 ;;(add-hook 'before-save-hook 'gofmt-before-save))
 (setq gofmt-command "goimports")
 (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'go-mode-setup)

(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-*")
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/go-mode")
(yas-global-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes nil)
 '(display-line-numbers nil)
 '(file-name-shadow-mode nil)
 '(inhibit-startup-screen t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   '(evil neotree direx peep-dired ranger avy nlinum-relative nlinum ag xref-js2 js2-refactor js2-mode go-rename magit markdown-mode go-eldoc go-autocomplete auto-complete flycheck go-mode))
 '(speedbar-show-unknown-files t))
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (defun return-indent ()
;;  (move-end-of-line)
;;  (<RET>))
;;
;; (global-set-key (kbd "M-j") 'return-indent)
;;
