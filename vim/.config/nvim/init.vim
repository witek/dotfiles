call plug#begin(stdpath('data') . '/plugged')

Plug 'vim-airline/vim-airline'
Plug 'gruvbox-community/gruvbox'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'Olical/aniseed', {'tag': 'v3.16.0'}
Plug 'Olical/conjure', {'tag': 'v4.17.0'}
Plug 'Olical/fennel.vim'
"TODO nvim-local-fennel
"
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'Olical/coc-conjure'

Plug 'liuchengxu/vim-which-key' 

Plug 'tpope/vim-surround'

Plug 'tpope/vim-vinegar'
Plug 'ryanoasis/vim-devicons'
Plug 'mbbill/undotree'
Plug 'vim-airline/vim-airline'
Plug 'tpope/vim-fugitive'



call plug#end()


" Load Defaults
source $HOME/.config/nvim/defaults.vim


" Plugins Configuration
source $HOME/.config/nvim/vim-which-key.vim
source $HOME/.config/nvim/ctrlp.vim
source $HOME/.config/nvim/coc.vim
source $HOME/.config/nvim/aniseed.vim


