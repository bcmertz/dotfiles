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

;; emacs as window manager
(setq session (getenv "SESSION"))
(if (equal session "emacs")
    (progn
      (message "managing windows")
      (load "custom-exwm-config.el")
      )
  )

;; debug startup performance / load time using (measure-time(load "custom-module.el"))
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; set customize config file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; general configuration - toggle evil with M-SPC; SPC is leader key
(measure-time(load "custom-general.el"))
(measure-time(load "custom-evil-keymap.el"))
(measure-time(load "custom-funcs.el"))
(measure-time(load "custom-styling.el"))
(measure-time(load "custom-autocomplete.el"))
(measure-time(load "custom-navigation.el"))
(measure-time(load "custom-editing.el"))
(measure-time(load "custom-bidi-text.el"))
(measure-time(load "custom-windowing.el"))
(measure-time(load "custom-flycheck.el"))
(measure-time(load "custom-email.el"))
(measure-time(load "custom-pass.el"))
(measure-time(load "custom-lsp.el"))
(measure-time(load "custom-git.el"))
(measure-time(load "custom-pdf.el"))
(measure-time(load "custom-compile.el"))

;; mode configurations
(measure-time(load "custom-golang.el"))
(measure-time(load "custom-js.el"))
(measure-time(load "custom-typescript.el"))
(measure-time(load "custom-markdown.el"))
(measure-time(load "custom-css.el"))
(measure-time(load "custom-bash.el"))
(measure-time(load "custom-html.el"))
(measure-time(load "custom-org.el"))
(measure-time(load "custom-latex.el"))

;; Return to normal gc value
(setq gc-cons-threshold 800000)
;; garbage collect when emacs leaves focus
(add-hook 'focus-out-hook 'garbage-collect)

;;; init.el ends here
