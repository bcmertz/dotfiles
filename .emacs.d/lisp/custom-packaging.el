;;; custom-packaging.el --- configure package management -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; package management with straight and use-package
;;;
;;; Code:

;; don't create *straight-process* buffer
;; (setq straight--process-log nil)

;; rebuild changed packages based on these conditions
(setq straight-check-for-modifications '(find-at-startup find-when-checking)) ;; rebuild changed packages on startup, slower
;; (setq straight-check-for-modifications '(watch-files find-when-checking)) ;; faster

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

;; initialize use-package
(straight-use-package 'use-package)

;; whether to automatically ensure every package exists (like :ensure or :straight)
;; not needed because of straight-use-package-by-default t which accomplishes
;; the same thing but through straight
(setq use-package-always-ensure nil)
(setq straight-use-package-by-default t) ;; always :straight t

;; use most up-to-date use-package integration syntax
(setq straight-use-package-version 'straight)

(provide 'custom-packaging)
;;; custom-packaging.el ends here
