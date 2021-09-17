nnoremap <silent> <leader> :silent WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :silent <c-u> :silent WhichKeyVisual '<Space>'<CR>

nnoremap <silent> <localleader> :silent WhichKey ','<CR>
vnoremap <silent> <localleader> :silent <c-u> :silent WhichKeyVisual ','<CR>


" Create map to add keys to
let g:which_key_map =  {}
" Define a separator
let g:which_key_sep = '→'
set timeoutlen=500


" Not a fan of floating windows for this
let g:which_key_use_floating_win = 0
let g:which_key_vertical = 0

" Change the colors if you want
highlight default link WhichKey          Operator
highlight default link WhichKeySeperator DiffAdded
highlight default link WhichKeyGroup     Identifier
highlight default link WhichKeyDesc      Function

" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Single mappings
let g:which_key_map[' '] = [ ':Commands'  , 'commands...' ]
let g:which_key_map['<Tab>'] = [ '<C-^>'  , 'other buffer' ]
let g:which_key_map['/'] = [ ':Telescope live_grep'  , 'search' ]
let g:which_key_map['e'] = [ ':CocCommand explorer'       , 'explorer' ]
"let g:which_key_map['f'] = [ ':Files'                     , 'search files' ]
let g:which_key_map['h'] = [ '<C-W>s'                     , 'split below']
let g:which_key_map['S'] = [ ':Startify'                  , 'start screen' ]


let g:which_key_map.f = {
      \ 'name' : '+files' ,
      \ 'n' : [':NERDTree' , 'NerdTree'],
      \ 'f' : [':CtrlP'    , 'search...'],
      \ 's' : [':w'        , 'save'],
      \ 't' : [':Telescope find_files'   , 'Telescope'],
      \ }

let g:which_key_map.s = {
      \ 'name' : '+search' ,
      \ '/' : [':History/'     , 'history'],
      \ ';' : [':Commands'     , 'commands'],
      \ 'a' : [':Ag'           , 'text Ag'],
      \ 'b' : [':BLines'       , 'current buffer'],
      \ 'B' : [':Buffers'      , 'open buffers'],
      \ 'c' : [':Commits'      , 'commits'],
      \ 'C' : [':BCommits'     , 'buffer commits'],
      \ 'f' : [':Files'        , 'files'],
      \ 'g' : [':GFiles'       , 'git files'],
      \ 'G' : [':GFiles?'      , 'modified git files'],
      \ 'h' : [':History'      , 'file history'],
      \ 'H' : [':History:'     , 'command history'],
      \ 'l' : [':Lines'        , 'lines'] ,
      \ 'm' : [':Marks'        , 'marks'] ,
      \ 'M' : [':Maps'         , 'normal maps'] ,
      \ 'p' : [':Helptags'     , 'help tags'] ,
      \ 'P' : [':Tags'         , 'project tags'],
      \ 's' : [':Snippets'     , 'snippets'],
      \ 'S' : [':Colors'       , 'color schemes'],
      \ 't' : [':Rg'           , 'text Rg'],
      \ 'T' : [':BTags'        , 'buffer tags'],
      \ 'w' : [':Windows'      , 'search windows'],
      \ 'y' : [':Filetypes'    , 'file types'],
      \ 'z' : [':FZF'          , 'FZF'],
      \ }

let g:which_key_map.t = {
      \ 'name' : '+toggle' ,
      \ 'u' : [':UndotreeToggle'   , 'undo tree'],
      \ }

let g:which_key_map.v = {
      \ 'name' : '+vim' ,
      \ '.' : [':so %'         , 'source buffer'],
      \ 'i' : [':e $MYVIMRC'   , 'open init.vim'],
      \ }

let g:which_key_map.v.p = {
      \ 'name' : '+Plug' ,
      \ 'c' : [':PlugClean'   , 'clean'],
      \ 'i' : [':PlugInstall' , 'install'],
      \ 'u' : [':PlugUpdate'  , 'update'],
      \ }

let g:which_key_map.w = {
      \ 'name' : '+windows' ,
      \ 'd' : ['<C-W>q'   , 'close'],
      \ 'm' : ['<C-W>o'   , 'maximize'],
      \ 'w' : ['<C-W>w'   , 'show next'],
      \ }

" Register which key map
call which_key#register('<Space>', "g:which_key_map")

