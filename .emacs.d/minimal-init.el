;; load paths
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set customize config file location and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file nil t)

(measure-time(require 'custom-packaging))        ;; repositories and package management
(measure-time(require 'custom-backups))            ;; custom functions used throughout our configuration
