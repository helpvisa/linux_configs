:so $VIMRUNTIME/defaults.vim

filetype plugin indent on
set number
set nowrap
set softtabstop=4
syntax on
set backspace=indent,eol,start
set cursorcolumn

"vimplug stuff
"automatically acquire vimplug
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"enable plugins
call plug#begin()
Plug 'dense-analysis/ale'
call plug#end()
