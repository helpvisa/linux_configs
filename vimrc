:so $VIMRUNTIME/defaults.vim

" general options
" set backspace=indent,eol,start
set number
set nowrap
set nocompatible
set showmatch
set ignorecase
set mouse=v
set hlsearch
set incsearch
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set autoindent
set number
set wildmode=longest,list
set cc=80
filetype plugin indent on
syntax on
set mouse=a
set clipboard=unnamedplus
filetype plugin on
set cursorline
set ttyfast
" set noshowmode

" block cursor in normal mode
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" automatically acquire vimplug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" enable plugins
call plug#begin("~/.vim/plugged")
    " Plug 'Shougo/deoplete.nvim'
    " Plug 'roxma/nvim-yarp'
    " Plug 'roxma/vim-hug-neovim-rpc'
    Plug 'dense-analysis/ale'
    Plug 'preservim/nerdtree'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'tpope/vim-commentary'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'itchyny/vim-cursorword'
call plug#end()
" let g:deoplete#enable_at_startup = 1
let g:ale_completion_enabled = 1

" set colorscheme
set termguicolors
colorscheme quiet

" highlight word under cursor (in vimscript)
" set updatetime=10
" function! HighlightWordUnderCursor()
"     if getline(".")[col(".")-1] !~# '[[:punct:][:blank:]]'
"         exec 'match' 'Search' '/\V\<'.expand('<cword>').'\>/'
"     else
"         match none
"     endif
" endfunction
" autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()

" remap keys
let mapleader = " "
" buffer navigation
nnoremap <leader>b :buffers<CR>:buffer<Space>
" nerdtree
nnoremap <leader>e :NERDTreeToggle<CR>
nnoremap <leader>Ef :NerdTreeFind<CR>
nnoremap <leader>EE :NerdTreeFocus<CR>
" indent guides
nnoremap <leader>ig :IndentGuidesToggle<CR>
" ale
nnoremap <leader>a :ALEToggle<CR>
nnoremap gd :ALEGoToDefinition<CR>
nnoremap gtd :ALEGoToTypeDefinition<CR>
nnoremap gr :ALEFindReferences<CR>
nnoremap gk :ALEHover<CR>
nnoremap <leader>ca :ALECodeAction<CR>
nnoremap <leader>rn :ALERename<CR>
nnoremap <leader>rf :ALEFileRename<CR>
" fzf
nnoremap <leader>o :Files<CR>
nnoremap <leader>, :Buffers<CR>
nnoremap <leader>m :Marks<CR>
nnoremap <leader>j :Jumps<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>/ :History/<CR>
nnoremap <leader>x :Commands<CR>
nnoremap <leader>g :Rg<CR>
nnoremap <leader>G :RG<CR>
nnoremap <leader>L :Lines<CR>
nnoremap <leader>l :BLines<CR>
