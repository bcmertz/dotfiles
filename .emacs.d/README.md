# .emacs.d

Heavily influenced by zamansky and clarete

Theme: atom-one-dark, spaceline mode-line
Navigation: avy, swiper, counsel, windmove, which-key, ido, neotree, smex
Editing: smartparens, multiple-cursors, magit
GO: go-mode, yasnippet, go-eldoc, company autocompletion, goimports, gocode backend
JS: js-mode, yasnippet, flycheck


# Key Bindings:

## Navigation

sp-show-enclosing-pair : C-<escape>
sp-up-sexp : M-<escape>
sp-down-sexp : C-M-<escape>
avy-goto-char-2 : M-g
avy-goto-line : M-l
swiper : C-s and C-r
counsel-ag : M-s
neotree-toggle : C-\\

## Editing

zap-to-char : M+z
return-newine-below : M-RET
mc/mark-next-like-this : "M-."
mc/mark-previous-like-this : "M-,"
mc/mark-all-like-this : "C-c M-."
mc/edit-lines : "C-c C-e"


## Window

enlarge-window : M-+
shrink-window  : M-_
enlarge-window-horizontally : M-=
shrink-window-horizontally :  M--

## GO

jump-to-definition : C-c C-j
go-rename          : C-c C-r

## JS

js-find-symbol : C-c C-f

## Markdown

flymd-flyit (preview) : C-c m