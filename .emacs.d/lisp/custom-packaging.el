;;; custom-packaging.el --- configure package management -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; package management with straight and use-package
;;;
;;; Code:

;; don't create *straight-process* buffer
;; (setq straight--process-log nil)
(setq straight-check-for-modifications '(watch-files find-when-checking))
;; (setq straight-check-for-modifications '(find-at-startup find-when-checking)) ;; rebuild changed packages on startup, slower
;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use use-package
(straight-use-package 'use-package)
;; automatically ensure every package exists (like :ensure or :straight)
;; (setq use-package-always-ensure t)


(setq straight-use-package-by-default t)

;; DEPRECATED Packages and repository management
;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("melpa-stable" . "https://stable.melpa.org/packages/")
;;                          ("org" . "https://orgmode.org/elpa/")
;;                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                          ("elpa" . "https://elpa.gnu.org/packages/")))

;; ;; (setq use-package-verbose t)

;; ;; unless we have use-package, refresh cache and install use-package
;; (unless (package-installed-p 'use-package)
;;   (progn
;;     (unless package-archive-contents
;;       (package-refresh-contents))
;;     (package-install 'use-package)))

;; (require 'use-package)

(provide 'custom-packaging)
;;; custom-packaging.el ends here
