;;; custom-typescript.el --- ts configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary: This is really just the web mode config but since we use it for tsx files primarily here it is
;;;
;;; Code:

(use-package typescript-mode
  :defer t
  :init
  (setq typescript-indent-level 2))

(provide 'custom-typescript)
;;; custom-typescript.el ends here
