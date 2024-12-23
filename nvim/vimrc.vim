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
" set noshowmode


" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" install plugins
call plug#begin("~/.vim/plugged")
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-tree/nvim-web-devicons'
    Plug 'nvim-tree/nvim-tree.lua'
    Plug 'nathanaelkane/vim-indent-guides'

    " colour schemes
    Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
    Plug 'kdheepak/monochrome.nvim'
    Plug 'pgdouyon/vim-yin-yang'
    Plug 'karoliskoncevicius/distilled-vim'

    " autocompletion and linting
    Plug 'neovim/nvim-lspconfig'
    Plug 'echasnovski/mini.nvim', { 'branch': 'stable' }
    " nvim-cmp + vsnip
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-vsnip'
    Plug 'hrsh7th/vim-vsnip'
call plug#end()

" set colorscheme
" colorscheme catppuccin-mocha
" colorscheme monochrome
" colorscheme distilled
colorscheme yin " or colorscheme yang
