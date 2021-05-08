;;; init.el --- loads various configuration layers
;;;
;;; Commentary:
;;;
;;; This is my personal config, based heavily on clarete, spacemacs, zamanksy, sam217pa
;;;
;;; Code:
(setq gc-cons-threshold most-positive-fixnum) ; Don't gc on startup

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))

;; debug startup performance / load time using (measure-time(load "custom-module.el"))
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; set customize config file location and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(measure-time(load custom-file))

(measure-time(load "custom-packaging.el"))        ;; repositories and package management
(measure-time(load "custom-funcs.el"))            ;; custom functions used throughout our configuration

(if (equal (getenv "SESSION") "emacs")            ;; see if SESSION env var is emacs
    (measure-time(load "custom-exwm-config.el"))) ;; emacs as window manager

(measure-time(load "custom-general.el"))          ;; general configuration
(measure-time(load "custom-evil-keymap.el"))      ;; vim emulation; toggle evil with M-SPC; SPC is leader key
(measure-time(load "custom-styling.el"))          ;; appearance
(measure-time(load "custom-autocomplete.el"))     ;; company autocompletion && snippets
(measure-time(load "custom-navigation.el"))       ;; navigating projects and code
(measure-time(load "custom-editing.el"))          ;; efficient text editing
(measure-time(load "custom-bidi-text.el"))        ;; rtl and ltr language support
(measure-time(load "custom-windowing.el"))        ;; buffer management
(measure-time(load "custom-flycheck.el"))         ;; error checking
(measure-time(load "custom-pass.el"))             ;; password management
;; (measure-time(load "custom-email.el"))         ;; email - mu4e
(measure-time(load "custom-lsp.el"))              ;; language server protocol
(measure-time(load "custom-git.el"))              ;; version control
(measure-time(load "custom-pdf.el"))              ;; pdf viewing/editing
(measure-time(load "custom-compile.el"))          ;; compilation

;; languages
(measure-time(load "custom-txt.el"))
(measure-time(load "custom-golang.el"))
(measure-time(load "custom-js.el"))
(measure-time(load "custom-typescript.el"))
(measure-time(load "custom-markdown.el"))
(measure-time(load "custom-css.el"))
(measure-time(load "custom-bash.el"))
(measure-time(load "custom-html.el"))
(measure-time(load "custom-org.el"))
(measure-time(load "custom-arduino.el"))
(measure-time(load "custom-latex.el"))

(setq gc-cons-threshold 800000)                ;; Return to normal gc value
(add-hook 'focus-out-hook 'garbage-collect)    ;; garbage collect when emacs leaves focus

;;; init.el ends here
