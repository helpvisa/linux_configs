:so $VIMRUNTIME/defaults.vim

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

" block cursor in normal mode, line cursor in insert mode
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" automatically acquire vimplug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" enable plugins
call plug#begin("~/.vim/plugged")
    Plug 'junegunn/goyo.vim'
    Plug 'dense-analysis/ale'
    Plug 'preservim/nerdtree'
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'tpope/vim-commentary'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    Plug 'itchyny/vim-cursorword'
    Plug 'tpope/vim-rsi'
    " Plug 'ludovicchabant/vim-gutentags'
    if has('patch-8.0.902')
      Plug 'mhinz/vim-signify'
    else
      Plug 'mhinz/vim-signify', { 'tag': 'legacy' }
    endif
    " fix clipboard on wayland
    Plug 'jasonccox/vim-wayland-clipboard'
    " colour schemes
    Plug 'NLKNguyen/papercolor-theme'
    Plug 'zefei/cake16'
    Plug 'Alligator/accent.vim'
call plug#end()
let g:ale_completion_enabled = 1
let g:ale_hover_to_floating_preview = 1
let g:ale_floating_preview = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %severity%: %s'
" set some specific linter preferences
let g:ale_linters = { 'python': ['pylsp'] }

" set colorscheme
set termguicolors
set background=dark
colorscheme PaperColor

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

" define a function to toggle word wrapping
let s:wrapenabled = 0
function! ToggleWrap()
    if s:wrapenabled
        set nolinebreak
        set nowrap
        let s:wrapenabled = 0
    else
        set linebreak
        set wrap
        let s:wrapenabled = 1
    endif
endfunction

" disable weird syntax highlighting and numbering when composing email in mutt
autocmd BufNewFile,BufRead /tmp/neomutt* set noautoindent filetype=mail linebreak wrap columns=80 tw=0 wm=0
autocmd BufNewFile,BufRead /tmp/mutt* set noautoindent filetype=mail linebreak wrap columns=80 tw=0 wm=0

" remap keys
let mapleader = " "
" toggle word wrap
map <leader>w :call ToggleWrap()<CR>
" quickly compile project
nnoremap <leader>b :make<CR>
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
