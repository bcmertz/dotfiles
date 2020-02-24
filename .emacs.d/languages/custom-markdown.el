;;;;;;;;;;;;;;;;;;;;;;; Markdown Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode "\\.md\\'"
  :defer t
  :config
  (custom-set-variables
  '(markdown-command "/usr/bin/pandoc"))
  )
