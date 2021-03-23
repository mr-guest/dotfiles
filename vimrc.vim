"Reneral
set history=500
filetype plugin on
filetype indent on 
set ruler
set number
set clipboard=unnamed
set ignorecase
set smartcase
set hlsearch
set fileformats=unix,dos,mac
"Color
syntax on
set encoding=utf8
colorscheme carbonized-dark
"backup turn off
set nobackup
set nowb
set noswapfile
"plugin
call plug#begin('~/.vimfiles/plugged')
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'jceb/vim-orgmode'
Plug 'nightsense/carbonized'
" python
"" Python Bundle
Plug 'raimon49/requirements.txt.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release' , 'for' : ['json','cpp','py','vim','css','html']	}
Plug 'tmhedberg/SimpylFold'
augroup vimrc-python
  autocmd!
  autocmd FileType python setlocal expandtab shiftwidth=4 tabstop=8 colorcolumn=79
      \ formatoptions+=croq softtabstop=4
      \ cinwords=if,elif,else,for,while,try,except,finally,def,class,with
augroup END
let python_highlight_all = 1
call plug#end()
