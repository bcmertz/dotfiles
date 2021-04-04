;;; custom-packaging.el --- configure libraries and packages
;;;
;;; Commentary:
;;;
;;; package management
;;;
;;; Code:

;; Packages and repository management
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; unless we have use-package, refresh cache and install selected
;; (when (require 'use-package nil 'noerror)
(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install-selected-packages)
    )
  )
        

;;; custom-packaging.el ends here
