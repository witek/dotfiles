
colorscheme gruvbox
set guifont=Fira\ Code:h12


let g:mapleader = "\<Space>"
let gmaplocalleader = ","


" tabs and indentation
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
set smartindent


" misc
set relativenumber
set nohlsearch
"set ignorecase
"set smartcase
set noswapfile
set nobackup
set nowritebackup
set incsearch
set termguicolors
set scrolloff=12
set signcolumn=yes
set colorcolumn=80
set hidden
set cmdheight=2
set encoding=utf-8
set nocompatible
set updatetime=300
set shortmess+=c

" enable system clipboard
set clipboard=unnamedplus


" colors and syntax highlighting
if (has("termguicolors"))
  set termguicolors
endif
syntax enable


