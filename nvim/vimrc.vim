" general options
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
set noshowmode


" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" install plugins
call plug#begin("~/.nvim/plugged")
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-tree/nvim-web-devicons'
    Plug 'nvim-tree/nvim-tree.lua'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'echasnovski/mini.nvim', { 'branch': 'stable' }

    " colour schemes
    Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
    Plug 'austinliuigi/smoke.nvim'

    " autocompletion and linting
    " Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'dense-analysis/ale'
call plug#end()
" let g:deoplete#enable_at_startup = 1
let g:ale_completion_enabled = 1
let g:ale_hover_to_floating_preview = 1

" set colorscheme
" colorscheme catppuccin-mocha
colorscheme smoke
