;;; minimal-init.el --- loads minimal configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; used for debugging when i break the regular config
;;;
;;; Code:

;; load paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set customize config file location and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

(require 'custom-packaging)        ;; repositories and package management
(require 'custom-backups)          ;; custom functions used throughout our configuration

;;; minimal-init.el ends here
