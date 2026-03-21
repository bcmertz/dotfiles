;;; custom-lsp.el --- golang configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; LSPs provide IDE like functionality - renaming variables, completion, error checking
;;; which are digested through builtin functions in our LSP client (eg - eglot-rename)
;;; or are digested by other libraries like flymake (error-checking) or corfu (completion)
;;;
;;; Code:

(use-package eglot
  :defer t
  :hook
  (python-mode . eglot-ensure)
  (bash-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l f" . eglot-format-buffer)
              ("C-c l F" . eglot-format)
              ("C-c l D" . eglot-find-declaration)
              ("C-c l d" . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l S" . eglot-server-menu)
              ("C-c l a" . eglot-code-actions)
              ("C-c l o" . eglot-code-actions-organize-imports)
              ("C-c l k" . eglot-shutdown)
              ("C-c l K" . eglot-shutdown-all)
              ("C-c l r" . eglot-rename)
              ("C-c l x" . eglot-code-actions-quickfix)
              ("C-c l h" . eglot-momentary-inlay-hints))
  :config
  (which-key-add-key-based-replacements "C-c l" "lsp")
  (add-to-list 'eglot-server-programs
               '(python-mode . ("rass" "python"))  ;; install `pipx install rassumfrassum ty ruff`
               ;; '(python-mode . ("ty" "server"))
               )
  (setq eglot-code-action-indications '()) ;; set how eglot lets you know there are code actions
  (setq eglot-autoshutdown t)  ; Shut down after killing last managed buffer
  ;; remove bold from occurrences of variable under cursor and type hints
  ;; silence long-running process notifications
  (setq eglot-report-progress nil)
  ;; don't use these eglot server features
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider :semanticTokensProvider))
  ;; workspace configuration
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:jedi_completion (:include_params t
                                                      :fuzzy t)
                                                     :pylint (:enabled :json-false)))
                         :gopls (:usePlaceholders t)))
  )

;; turn off JSONRPC debug event log mechanism.
;; helps speed up eglot
;; https://www.reddit.com/r/emacs/comments/1447fy2/looking_for_help_in_improving_typescript_eglot/
;; maybe no longer necessary as of v30?
;; https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots/
;;(fset #'jsonrpc--log-event #'ignore)

(provide 'custom-lsp)
;;; custom-lsp.el ends here
