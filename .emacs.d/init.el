;;; init.el --- loads various configuration layers -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; This is my personal config, based heavily on clarete, spacemacs, zamanksy, sam217pa
;;;
;;; Code:

;; load paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))

;; debug startup performance / load time using (measure-time(load "custom-module.el"))
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time))
         (name (format "%s" ,@body)))
     ,@body
     (message "%-24s %.03fs" (concat name "...") (float-time (time-since time)))))

;; set customize config file location and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

(measure-time(require 'custom-packaging))        ;; repositories and package management
(measure-time(require 'custom-funcs))            ;; custom functions used throughout our configuration

(if (equal (getenv "SESSION") "emacs")            ;; see if SESSION env var is emacs
    (measure-time(require 'custom-exwm-config))) ;; emacs as window manager

(measure-time(require 'custom-splashscreen))     ;; splash-screen
(measure-time(require 'custom-backups))          ;; backups and temp files
(measure-time(require 'custom-general))          ;; general configuration
(measure-time(require 'custom-keybindings))      ;; evil, suggestions
(measure-time(require 'custom-styling))          ;; appearance
(measure-time(require 'custom-scrolling))        ;; scrolling
(measure-time(require 'custom-fonts))            ;; fonts
(measure-time(require 'custom-modeline))         ;; mode line
(measure-time(require 'custom-completion))       ;; completion && snippets
;; (measure-time(require 'custom-treesitter))       ;; tree-sitter
(measure-time(require 'custom-project))          ;; project management and navigation
(measure-time(require 'custom-minibuffer))       ;; vertico consult marginalia embark family
(measure-time(require 'custom-dired))            ;; dired
(measure-time(require 'custom-sidebar))          ;; sidebar
(measure-time(require 'custom-editing))          ;; efficient text editing
(measure-time(require 'custom-tramp))            ;; tramp
(measure-time(require 'custom-bidi-text))        ;; rtl and ltr language support
(measure-time(require 'custom-windowing))        ;; window & buffer management
(measure-time(require 'custom-error-check))      ;; error checking
(measure-time(require 'custom-pass))             ;; password management
(measure-time(require 'custom-help))             ;; help
;; (measure-time(require 'custom-email))         ;; email - mu4e
;; (measure-time(require 'custom-music))         ;; music - emms
(measure-time(require 'custom-lsp))              ;; language server protocol
(measure-time(require 'custom-terminal))         ;; terminal
(measure-time(require 'custom-org))              ;; org mode
(measure-time(require 'custom-git))              ;; version control
(measure-time(require 'custom-pdf))              ;; pdf viewing/editing
(measure-time(require 'custom-compile))          ;; compilation
(measure-time(require 'custom-menus))            ;; mouse interaction and menus

;; languages
(measure-time(require 'custom-txt))
(measure-time(require 'custom-golang))
(measure-time(require 'custom-js))
(measure-time(require 'custom-vue))
(measure-time(require 'custom-typescript))
(measure-time(require 'custom-python))
(measure-time(require 'custom-markdown))
(measure-time(require 'custom-css))
(measure-time(require 'custom-bash))
(measure-time(require 'custom-html))
(measure-time(require 'custom-arduino))
(measure-time(require 'custom-latex))

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (add-hook 'focus-out-hook 'garbage-collect)
            (setq gc-cons-threshold (expt 2 23))))

;;; init.el ends here
