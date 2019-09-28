;;;;;;;;;;;;;;;;;;;;;;; Markdown Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flymd
  :ensure t
  :bind ("C-c m" . flymd-flyit)) ; previews

(setq flymd-output-directory "/tmp")

;; Firefox for compatability
(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)
