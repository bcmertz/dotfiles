call plug#begin('~/.vim/plugged')
Plug 'mbbill/undotree'
Plug 'preservim/nerdtree'
Plug 'joshdick/onedark.vim'
call plug#end()

" General Configuration
set number        " line numbers
syntax on         " syntax highlighting
set autoindent    " autoindent
set expandtab     " expand tabs to spaces
set softtabstop=4
set nocompatible  " enter 21st cenuury
set shortmess+=A  " dont warn swapfile already exists

" Key Maps
map <C-o> :NERDTreeToggle<CR>
