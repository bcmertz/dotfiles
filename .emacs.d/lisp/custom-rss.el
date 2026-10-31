;;; custom-rss.el --- rss configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; rss
;;;
;;; Code:

(use-package elfeed
  :defer t
  :hook (elfeed-search . elfeed-update)
  :bind ((:map elfeed-search-mode-map
               ("C-\\" . elfeed-tree))
         (:map elfeed-tree-mode-map
              ("C-\\" . quit-window)))
  :config
  (setq elfeed-set-max-connections 5)
  (setq elfeed-feeds
        '(("https://karthinks.com/tags/emacs/index.xml" :title "karthinks" emacs)
          ("https://sachachua.com/blog/category/emacs-news/feed/index.xml" :title "sachachua" emacs)
          ;; ("https://planet.emacslife.com/atom.xml" emacs) ;; ALL emacs blogs
          )))

(provide 'custom-rss)
;;; custom-rss.el ends here
