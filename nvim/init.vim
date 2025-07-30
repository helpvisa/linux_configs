" general options
filetype plugin on " set some plugins for certain filetypes
filetype plugin indent on " set some indentation plugins for certain filetypes
syntax on " enable syntax highlighting
set hidden " prevent buffer unloading when invisible
set number " show line numbers
set relativenumber " relative line numbers
set linebreak " break lines on word boundaries
set wrap " wrap lines
set ignorecase " no case sensitivity in search patterns by default
set mouse=a " enable the mouse in all modes
" set hlsearch " highlight all search pattern matches
set incsearch " incrementally search while typing a pattern
set tabstop=8 " default expansion width for tab character
set expandtab " expand tabs with space characters instead
set shiftwidth=4 " default indentation width
set softtabstop=-1 " set space-expanded tab width (-1 uses value of shiftwidth)
set autoindent " indent automagically
set wildmode=longest:full,full " make tab-complete behaviour similar to bash
set cc=80 " set line length guide to 80 (colour column)
set clipboard+=unnamedplus " expand clipboard to include system clipboard
set updatetime=300 " default timeout before cursor updates
set noequalalways " do not mess with split sizes when a preview window closes
set path+=** " recursively search subdrectories of current dir for find

" set default file browser layout, open files in previous split / window
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4

" automatically acquire vimplug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source "~/.config/nvim/init.vim"
endif

" enable plugins
call plug#begin("~/.config/nvim/plugged")
    " quick commenting
    Plug 'tpope/vim-commentary'
    " enable readline commands in insert mode
    Plug 'tpope/vim-rsi'
call plug#end()

" create an undo directory if it does not already exist
" we do this after .vim is created by vimplug
if !isdirectory($HOME . "/.config/nvim/undo_dir")
    call mkdir($HOME . "/.config/nvim/undo_dir", "p")
endif
set undodir=~/.config/nvim/undo_dir " set an undodir for persistent undo
set undofile                        " enable the persistent undo

" set colorscheme
" set termguicolors
colorscheme shine

"" CUSTOM FUNCTIONS
" toggle word wrapping
let s:wrapenabled = 1
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

" pipe shell command output into new buffer
function! ShellToBuffer(newtab)
    let l:cmd = input("Shell Command: ")
    if len(l:cmd > 0)
        if a:newtab < 1
            exe "noswapfile enew"
        elseif a:newtab == 2
            exe "noswapfile new"
        elseif a:newtab == 3
            exe "noswapfile vnew"
        else
            exe "noswapfile $tabnew"
        endif
        exe "set buftype=nofile"
        exe "set bufhidden=wipe"
        exe "0r!" . l:cmd
    endif
endfunction

" pipe internal VIM command output into new buffer
function! VimCmdToBuffer(newtab)
    let l:cmd = input("Vim Command: ")
    if len(l:cmd > 0)
        let @" = execute(l:cmd)
        if a:newtab < 1
            exe "noswapfile enew"
        elseif a:newtab == 2
            exe "noswapfile new"
        elseif a:newtab == 3
            exe "noswapfile vnew"
        else
            exe "noswapfile $tabnew"
        endif
        exe "set buftype=nofile"
        exe "set bufhidden=wipe"
        exe '0put"'
    endif
endfunction

" execute a system command and populate cexpr with it
function! PopulateCexprFromShell()
    let l:cmd = input("Shell command for cexpr: ")
    if len(l:cmd > 0)
        exe "cexpr system('" . l:cmd . "')"
    endif
endfunction

" disable weird syntax highlighting and numbering when composing email in mutt
autocmd BufNewFile,BufRead /tmp/neomutt* set noautoindent filetype=mail linebreak wrap columns=80 tw=0 wm=0
autocmd BufNewFile,BufRead /tmp/mutt* set noautoindent filetype=mail linebreak wrap columns=80 tw=0 wm=0

" remap keys
let mapleader = " "
" toggle word wrap
nnoremap <leader>w :call ToggleWrap()<CR>
" set up a quick vimgrep mapping
nnoremap <leader>g :vimgrep 
"sus shell or vim cmd and save output in scratch buffer
nnoremap <leader>se :call ShellToBuffer(0)<CR>
nnoremap <leader>ve :call VimCmdToBuffer(0)<CR>
nnoremap <leader>st :call ShellToBuffer(1)<CR>
nnoremap <leader>vt :call VimCmdToBuffer(1)<CR>
nnoremap <leader>ss :call ShellToBuffer(2)<CR>
nnoremap <leader>vs :call VimCmdToBuffer(2)<CR>
nnoremap <leader>sv :call ShellToBuffer(3)<CR>
nnoremap <leader>vv :call VimCmdToBuffer(3)<CR>
" work with the quickfix list
nnoremap <leader>co :copen<CR>
nnoremap <leader>cc :cclose<CR>
nnoremap <leader>cn :cn<CR>
nnoremap <leader>cp :cp<CR>
nnoremap <leader>cr :call PopulateCexprFromShell()<CR>
nnoremap <leader>cg :call PopulateCexprFromShell()<CR>grep -nHR 
" buffer jumps
nnoremap <leader>b :buffers<CR>:b<space>
" terminal
nnoremap <leader>ot :term<CR>
nnoremap <leader>ovt :vert term<CR>
