;;;;;;;;;;;;;;;;;;;;;;; Markdown Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode "\\.md\\'"
  :defer t
  :config
  (custom-set-variables
  '(markdown-command "/usr/bin/pandoc"))
  )

;; :bind ("C-c m" . flymd-flyit)) ; previews
;; (setq flymd-output-directory "/tmp")

;; ;; Firefox for compatability
;; (defun my-flymd-browser-function (url)
;;   (let ((browse-url-browser-function 'browse-url-firefox))
;;     (browse-url url)))
;; (setq flymd-browser-open-function 'my-flymd-browser-function)
