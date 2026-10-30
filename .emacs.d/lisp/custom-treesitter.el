;;; custom-treesitter.el --- tree-sitter configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; tree-sitter uses defined language grammars to have understanding of the syntax
;;; of a program, allowing for nice syntax highlighting and structural editing
;;;
;;; Code:

(require 'language-mode-info)

(use-package treesit
  :defer t
  :straight (:type built-in)
  ;; TODO
  ;; :config
  ;; (if (not (version< emacs-version "31.1"))
  ;;     (progn
  ;;       (setq treesit-auto-install-grammar 'ask)
  ;;       (setq treesit-enabled-modes t)))
  )

(use-package treesit-auto
  ;; TODO
  ;; :if (version< emacs-version "31.1")
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(defun treesit-lang-for-each (fn)
  "For each treesit language apply FN."
  (cl-loop for lang in language-list
	   do
           (if (listp (language-map-mode lang))
               (cl-loop for mode in (language-map-mode lang)
                        do
                        (let ((from mode)
		              (to (language-map-ts-mode lang)))
	                  (funcall fn from to)))
	     (let ((from (language-map-mode lang))
		   (to (language-map-ts-mode lang)))
	       (funcall fn from to)))))

(defun treesit-mode-hook-symbol (mode)
  "Return MODE hook name."
  (intern (concat (symbol-name mode) "-hook")))

(defvar treesit-run-original-hooks t)

;; this takes a non-treesitter major mode's hooks and applies it to the ts mode
;;
;; adapted from https://github.com/renzmann/treesit-auto/issues/52
(treesit-lang-for-each
 (lambda (from to)
   (interactive)
   (let ((targets (if (listp from) from (list from))))
     (cl-loop for from in targets
              do
              (letrec ((to-hook (treesit-mode-hook-symbol to))
	               (from-hook (treesit-mode-hook-symbol from))
                       (treesit-hook-name (intern (concat "treesit-" (symbol-name from-hook)))))
                (defalias treesit-hook-name
                  `(lambda ()
                     (when (and treesit-run-original-hooks
                                (boundp ',from-hook))
                       (message "Running hooks from %s for %s" ',from-hook ',to)
                       (run-hooks ',from-hook))))
                (add-hook to-hook treesit-hook-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ts code folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package ts-fold
;;   :straight (ts-fold :type git
;;                      :host github
;;                      :repo "emacs-tree-sitter/ts-fold")
;;   :init
;;   (global-ts-fold-mode)
;;   (global-ts-fold-indicators-mode)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ts code movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package combobulate
;;   ;; Ensure `combobulate-mode` is activated when you launch a mode it supports
;;   :hook ((python-mode . combobulate-mode)
;;          (js-mode . combobulate-mode)
;;          (typescript-mode . combobulate-mode))
;;   :straight (combobulate :type git
;;                          :host github
;;                          :repo "mickeynp/combobulate")
;;   )

(provide 'custom-treesitter)
;;; custom-treesitter.el ends here
