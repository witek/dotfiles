
call plug#begin('~/.config/nvim/plugged')
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'vim-airline/vim-airline'

  Plug 'nvim-telescope/telescope.nvim'
  "Plug 'francoiscabrol/ranger.vim'

  Plug 'tpope/vim-vinegar'
  Plug 'airblade/vim-rooter'
  "Plug 'qpkorr/vim-renamer'
  Plug 'mhinz/vim-startify'
  "Plug 'dracula/vim'
  Plug 'gruvbox-community/gruvbox'
  Plug 'scrooloose/nerdtree'
  Plug 'rbgrouleff/bclose.vim'
  Plug 'ryanoasis/vim-devicons'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
  Plug 'guns/vim-sexp',    {'for': 'clojure'}
  Plug 'bakpakin/fennel.vim'
  Plug 'mbbill/undotree'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'Olical/conjure', {'tag': 'v4.14.1'}
  "Plug 'liquidz/vim-iced', {'for': 'clojure'}
  Plug 'liuchengxu/vim-which-key'
call plug#end()


" tabs and indentation
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
set smartindent


" use system clipboard
set clipboard=unnamedplus

" misc
set relativenumber
set nohlsearch
"set ignorecase
"set smartcase
set noswapfile
set nobackup
set incsearch
set termguicolors
set scrolloff=12
set signcolumn=yes
set colorcolumn=80
set hidden
set cmdheight=2

" leader keys
let g:mapleader = "\<Space>"
let g:maplocalleader = ','


" colors and syntax highlighting
if (has("termguicolors"))
 set termguicolors
endif
syntax enable
colorscheme gruvbox
"highlight Normal guibg=none


" font with ligatures
set guifont=Fira\ Code:h10

" plugins
source $HOME/.config/nvim/nerdtree.vim
source $HOME/.config/nvim/vim-iced.vim
source $HOME/.config/nvim/undotree.vim
source $HOME/.config/nvim/vim-which-key.vim

