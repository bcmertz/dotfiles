;;; language-mode-info.el --- language and mode information -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; This provides a mapping of programming languages to file extensions, mode names,
;;; and typescript mode names, as well as facilities for converting between them.
;;; Inspired by treesit-auto functionality.
;;;
;;; Code:

(require 'cl-lib)

(cl-defstruct language-map
  "Mapping of a programming language, mode, typescript mode, and file extension."
  lang mode ts-mode ext)

(defvar language-list
  `(,(make-language-map
      :lang "nil"
      :mode "nil"
      :ts-mode "nil"
      :ext "nil")
    ,(make-language-map
      :lang "awk"
      :mode "awk-mode"
      :ts-mode "awk-ts-mode"
      :ext "\\.awk\\'")
    ,(make-language-map
      :lang "bash"
      :mode "bash-mode"
      :ts-mode "bash-ts-mode"
      :ext "\\.sh\\'")
    ,(make-language-map
      :lang "bibtex"
      :mode "bibtex-mode"
      :ts-mode "bibtex-ts-mode"
      :ext "\\.bib\\'")
    ,(make-language-map
      :lang "c"
      :mode "c-mode"
      :ts-mode "c-ts-mode"
      :ext "\\.c\\'")
    ,(make-language-map
      :lang "c-sharp"
      :mode "csharp-mode"
      :ts-mode "csharp-ts-mode"
      :ext "\\.cs\\'")
    ,(make-language-map
      :lang "clojure"
      :mode "(clojure-mode clojurescript-mode clojurec-mode)"
      :ts-mode "clojure-ts-mode"
      :ext "\\.cljc?s?d?\\'")
    ,(make-language-map
      :lang "cmake"
      :mode "cmake-mode"
      :ts-mode "cmake-ts-mode"
      :ext "\\.cmake\\'")
    ,(make-language-map
      :lang "commonlisp"
      :mode "common-lisp-mode"
      :ts-mode "commonlisp-ts-mode"
      :ext "\\.cl\\'")
    ,(make-language-map
      :lang "cpp"
      :mode "c++-mode"
      :ts-mode "c++-ts-mode"
      :ext "\\.cpp\\'")
    ,(make-language-map
      :lang "css"
      :mode "css-mode"
      :ts-mode "css-ts-mode"
      :ext "\\.css\\'")
    ,(make-language-map
      :lang "dart"
      :mode "dart-mode"
      :ts-mode "dart-ts-mode"
      :ext "\\.dart\\'")
    ,(make-language-map
      :lang "dockerfile"
      :mode "dockerfile-mode"
      :ts-mode "dockerfile-ts-mode"
      :ext "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")
    ,(make-language-map
      :lang "elisp"
      :mode "emacs-lisp-mode"
      :ts-mode "il"
      :ext "\\.el\\'")
    ,(make-language-map
      :lang "elixir"
      :mode "elixir-mode"
      :ts-mode "elixir-ts-mode"
      :ext "\\.ex\\'")
    ,(make-language-map
      :lang "go"
      :mode "go-mode"
      :ts-mode "go-ts-mode"
      :ext "\\.go\\'")
    ,(make-language-map
      :lang "gomod"
      :mode "go-mod-mode"
      :ts-mode "go-mod-ts-mode"
      :ext "go\\.mod\\'")
    ,(make-language-map
      :lang "html"
      :mode "(mhtml-mode sgml-mode)"
      :ts-mode "html-ts-mode"
      :ext "\\.html\\'")
    ,(make-language-map
      :lang "java"
      :mode "java-mode"
      :ts-mode "java-ts-mode"
      :ext "\\.java\\'")
    ,(make-language-map
      :lang "javascript"
      :mode "(js-mode javascript-mode js2-mode)"
      :ts-mode "js-ts-mode"
      :ext "\\.js\\'")
    ,(make-language-map
      :lang "json"
      :mode "js-json-mode"
      :ts-mode "json-ts-mode"
      :ext "\\.json\\'")
    ,(make-language-map
      :lang "julia"
      :mode "julia-mode"
      :ts-mode "julia-ts-mode"
      :ext "\\.jl\\'")
    ,(make-language-map
      :lang "kotlin"
      :mode "kotlin-mode"
      :ts-mode "kotlin-ts-mode"
      :ext "\\.kts?\\'")
    ,(make-language-map
      :lang "latex"
      :mode "latex-mode"
      :ts-mode "latex-ts-mode"
      :ext "\\.tex\\'")
    ,(make-language-map
      :lang "lua"
      :mode "lua-mode"
      :ts-mode "lua-ts-mode"
      :ext "\\.lua\\'")
    ,(make-language-map
      :lang "make"
      :mode "makefile-mode"
      :ts-mode "makefile-ts-mode"
      :ext "\\([Mm]akefile\\|.*\\.\\(mk\\|make\\)\\)\\'")
    ,(make-language-map
      :lang "markdown"
      :mode "(poly-markdown-mode markdown-mode)"
      :ts-mode "markdown-ts-mode"
      :ext "\\.md\\'")
    ,(make-language-map
      :lang "org"
      :mode "org-mode"
      :ts-mode "org-ts-mode"
      :ext "\\.org\\'")
    ,(make-language-map
      :lang "perl"
      :mode "perl-mode"
      :ts-mode "perl-ts-mode"
      :ext "\\.pl\\'")
    ,(make-language-map
      :lang "python"
      :mode "python-mode"
      :ts-mode "python-ts-mode"
      :ext "\\.py[itw]?\\'")
    ,(make-language-map
      :lang "r"
      :mode "ess-mode"
      :ts-mode "r-ts-mode"
      :ext "\\.r\\'")
    ,(make-language-map
      :lang "ruby"
      :mode "ruby-mode"
      :ts-mode "ruby-ts-mode"
      :ext "\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")
    ,(make-language-map
      :lang "rust"
      :mode "rust-mode"
      :ts-mode "rust-ts-mode"
      :ext "\\.rs\\'")
    ,(make-language-map
      :lang "tsx"
      :mode "(typescript-tsx-mode)"
      :ts-mode "tsx-ts-mode"
      :ext "\\.tsx\\'")
    ,(make-language-map
      :lang "typescript"
      :mode "typescript-mode"
      :ts-mode "typescript-ts-mode"
      :ext "\\.ts\\'")
    ,(make-language-map
      :lang "typst"
      :mode "typst-mode"
      :ts-mode "typst-ts-mode"
      :ext "\\.typ\\'")
    ,(make-language-map
      :lang "vue"
      :mode "vue-mode"
      :ts-mode "vue-ts-mode"
      :ext "\\.vue\\'")
    ,(make-language-map
      :lang "yaml"
      :mode "yaml-mode"
      :ts-mode "yaml-ts-mode"
      :ext "\\.ya?ml\\'")))

(defun current-mode ()
  "Find current mode from first matching method found in `language-list'."
  (seq-find (lambda (r)
              (if-let ((out (string-match (language-map-mode r) (symbol-name major-mode))))
                  out
                (if-let ((out (string-match (language-map-ts-mode r) (symbol-name major-mode))))
                    out
                  (if-let ((out (string-match (language-map-ext r) (buffer-name))))
                      out
                    nil))))
            language-list))

(defun buffer-lang ()
  "Return language corresponding to buffer mode."
  (message "%s" (current-mode))
  (language-map-lang (current-mode)))
(defun buffer-mode ()
  "Return non-typescript mode of current buffer."
  (language-map-mode (current-mode)))
(defun buffer-ts-mode ()
  "Return typescript mode of current buffer."
  (language-map-ts-mode (current-mode)))
(defun buffer-extension ()
  "Return typical file extension pattern of current buffer mode."
  (language-map-ext (current-mode)))


(provide 'language-mode-info)
;;; language-mode-info.el ends here
