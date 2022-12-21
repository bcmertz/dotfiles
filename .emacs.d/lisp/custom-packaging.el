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
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq use-package-always-ensure t)
;; (setq use-package-verbose t)

;; unless we have use-package, refresh cache and install use-package
(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)

;;; custom-packaging.el ends here
