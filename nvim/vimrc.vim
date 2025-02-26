" general options
filetype plugin on " set some plugins for certain filetypes
filetype plugin indent on " set some indentation plugins for certain filetypes
syntax on " enable syntax highlighting
set hidden " prevent buffer unloading when invisible
set number " show line numbers
set nowrap " do not wrap lines
set ignorecase " no case sensitivity in search patterns by default
set mouse=a " enable the mouse in all modes
set hlsearch " highlight all search pattern matches
set incsearch " incrementally search while typing a pattern
set tabstop=8 " default expansion width for tab character
set softtabstop=4 " set space-expanded tab width (4 spaces)
set expandtab " expand tabs with space characters instead
set shiftwidth=4 " default indentation width
set autoindent " indent automagically
set wildmode=longest,list " make tab-complete behaviour similar to bash
set cc=80 " set line length guide to 80 (colour column)
set clipboard+=unnamedplus " expand clipboard to include system clipboard
set cursorline " highlight the line currently occupied by the cursor
set updatetime=100 " default timeout before updates; lowered for some plugins
set noequalalways " do not mess with split sizes when a preview window closes
" insert-mode omnicompletion setup; trigger with C-x C-o when inserting
set omnifunc=ale#completion#OmniFunc


" auto-install vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" enable plugins
call plug#begin("~/.nvim/plugged")
    Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
    Plug 'nvim-tree/nvim-web-devicons'
    Plug 'nvim-tree/nvim-tree.lua'
    Plug 'junegunn/goyo.vim'
    Plug 'dense-analysis/ale'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'echasnovski/mini.nvim', { 'branch': 'stable' }
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'ludovicchabant/vim-gutentags'

    " colour schemes
    Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
    Plug 'austinliuigi/smoke.nvim'
call plug#end()
" let g:deoplete#enable_at_startup = 1
let g:ale_completion_enabled = 1
let g:ale_hover_to_floating_preview = 1
let g:ale_floating_preview = 1
" set some specific linter preferences
let g:ale_linters = { 'python': ['pylsp'] }

" set colorscheme
set background=dark
colorscheme catppuccin-frappe
set guifont=Input\ Mono\ 12

" remap keys
let mapleader = " "
" quickly compile project
nnoremap <leader>b :make<CR>
" window navigation
nnoremap <leader>w <c-w>
" nerdtree
nnoremap <leader>e :NvimTreeToggle<CR>
" indent guides
nnoremap <leader>ig :IndentGuidesToggle<CR>
" ale
nnoremap <leader>a :ALEToggle<CR>
nnoremap gd :ALEGoToDefinition<CR>
nnoremap gtd :ALEGoToTypeDefinition<CR>
nnoremap gr :ALEFindReferences<CR>
nnoremap gk :ALEHover<CR>
nnoremap gb :ALEDetail<CR>
nnoremap <leader>ca :ALECodeAction<CR>
nnoremap <leader>rn :ALERename<CR>
nnoremap <leader>rf :ALEFileRename<CR>
" fzf
nnoremap <leader>of :Files<CR>
nnoremap <leader>, :Buffers<CR>
nnoremap <c-w>, :Buffers<CR>
nnoremap <leader>m :Marks<CR>
nnoremap <leader>j :Jumps<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>/ :History/<CR>
nnoremap <leader>x :Commands<CR>
nnoremap <leader>g :Rg<CR>
nnoremap <leader>G :RG<CR>
nnoremap <leader>L :Lines<CR>
nnoremap <leader>l :BLines<CR>
nnoremap <leader>t :Tags<CR>
nnoremap <leader>T :BTags<CR>
" clear highlight
nnoremap <leader>c :noh<CR>
" terminal
nnoremap <leader>ot :term<CR>
nnoremap <leader>ovt :vert term<CR>
" move lines up/down
vnoremap <C-q> :m '<-2<CR>gv=gv
vnoremap <C-a> :m '>+1<CR>gv=gv
