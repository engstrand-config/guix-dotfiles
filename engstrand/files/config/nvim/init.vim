"---------------------------------------
"                Plugins
" --------------------------------------
if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ~/.config/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
endif

call plug#begin(stdpath('data') . '/plugged')

Plug 'mhinz/vim-startify'

Plug 'dylanaraps/wal.vim'
Plug 'itchyny/lightline.vim'
Plug 'neoclide/coc.nvim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'alvan/vim-closetag'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'Raimondi/delimitMate'
Plug 'christoomey/vim-sort-motion'
Plug 'honza/vim-snippets'
Plug 'takac/vim-hardtime'

" Language specific
Plug 'lervag/vimtex'
Plug 'heavenshell/vim-jsdoc', {
  \ 'for': ['javascript', 'javascript.jsx','typescript'],
  \ 'do': 'make install'
\}
Plug 'neovimhaskell/haskell-vim'
Plug 'evanleck/vim-svelte', {'branch': 'main'}
Plug 'elixir-editors/vim-elixir'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

call plug#end()


" --------------------------------------
"             Vim config
" --------------------------------------
colorscheme wal

set bg=dark
set go=a
set mouse=a
set nohlsearch
set clipboard=unnamedplus
set wildmenu
set nocompatible
set encoding=utf-8
set number relativenumber
set splitbelow splitright
set title

" No need to show '-- INSERT --' because of lightline
set noshowmode

set signcolumn=no
set updatetime=300

set softtabstop=4
set shiftwidth=4
set expandtab

filetype plugin on
filetype plugin indent on
syntax on

" --------------------------------------
"             General config
" --------------------------------------
let mapleader = ","
let b:signcolumn_on=0
let g:tex_flavor='latex'
let g:hardtime_default_on = 1
let g:list_of_normal_keys = ["h", "l", "-", "+", "<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:list_of_visual_keys = ["-", "+", "<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:vimtex_compiler_latexmk_engine='xelatex'
let g:vimtex_compiler_method='latexmk'
let g:vimtex_view_method='zathura'
let g:svelte_preprocessor_tags = [
  \ { 'name': 'ts', 'tag': 'script', 'as': 'typescript' }
  \ ]
let g:svelte_preprocessors = ['ts']
let g:lightline = {
      \ 'colorscheme': 'wal',
    \ }
let g:lightline.separator = {
    \   'left': '', 'right': ''
    \}
let g:lightline.subseparator = {
    \   'left': '|', 'right': '|'
    \}
let g:lightline.tabline = {
    \ 'left': [ [ 'tabs' ] ],
    \ 'right': [ ] }
let g:lightline.tabline_separator = {
    \   'left': '', 'right': ''
    \}
let g:lightline.tabline_subseparator = {
    \   'left': '', 'right': ''
    \}

" Open in new tab by default
let g:fzf_action = { 'enter': 'tab split' }

" --------------------------------------
"              Coc settings
" --------------------------------------
" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

" Start autocomplete/move down list
inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

" Move up in autocomplete list
inoremap <silent><expr> <S-Tab>
      \ pumvisible() ? "\<C-p>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> <leader>d :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Expand snippet on enter
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() :
                                           \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use Alt+Tab to move forward in snippet
let g:coc_snippet_next = '<M-Tab>'


" --------------------------------------
"                Remaps
" --------------------------------------
nnoremap c "_c

" JsDoc
nmap <silent> <C-l> <Plug>(jsdoc)

" Fuzzy finder
nnoremap <silent> <leader>o :GFiles<CR>

" Replace all is aliased to S.
nnoremap S :%s//g<Left><Left>

" Replace all occurences of word under cursor
nnoremap <leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>

" Shortcut for finding a parenthesis and changing its content from anywhere on a line
nnoremap <leader>p %ci(

" Shortcut for formatting document
nnoremap <leader>fi gg=G<CR>

" Compile Haskell file using GHCi
nnoremap <leader>hb :!ghci %<CR>

" Toggles the workspace on/off
nnoremap <leader>w :ToggleWorkspace<CR>

" Toggle gutter
function! ToggleSignColumn()
    if !exists("b:signcolumn_on") || b:signcolumn_on
        set signcolumn=no
        let b:signcolumn_on=0
    else
        set signcolumn=auto
        let b:signcolumn_on=1
    endif
endfunction
nnoremap <leader>gs :call ToggleSignColumn()<CR>

" Copy selected text to system clipboard (requires gvim/nvim/vim-x11 installed):
vnoremap <C-c> "+y
map <C-p> "+P

" Disable the arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

inoremap <Up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

" --------------------------------------
"              Color fixes
" --------------------------------------
highlight Normal ctermbg=none
highlight NonText ctermbg=none

let s:palette = g:lightline#colorscheme#wal#palette
let s:palette.tabline.tabsel = [ [ 'NONE', 'NONE', 'NONE', 'NONE', 'bold'] ]

" Change the color of comments
hi Comment ctermfg=9

" Change color of line numbers
hi LineNr ctermfg=1

" Change color of selected line number
hi CursorLineNr ctermfg=7

" Change the background color of highlighted matching tags
hi MatchParen ctermbg=2 ctermfg=0

" Change the colors for the dropdown menu for autocomplete
hi Pmenu ctermbg=0 ctermfg=3

" Fix the coloring of ; and : in CSS-files
hi CssNoise ctermfg=4

" This line enables the true color support.
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

let g:vimtex_compiler_latexmk = {
    \ 'options' : [
    \   '-pdf',
    \   '-shell-escape',
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \ ],
    \}

let g:fzf_colors =
\ { 'bg':      ['bg', 'Normal'],
  \ 'border':  ['bg', 'Normal'] }
let g:fzf_layout = { 'down': '50%' }

" --------------------------------------
"              Autocommands
" --------------------------------------
command Todo noautocmd vimgrep /TODO\|FIXME/j ** | cw

" set filetypes as typescriptreact
autocmd BufNewFile,BufRead *.tsx,*.jsx set filetype=typescriptreact

" Automatically deletes all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Automatic line wrapping
autocmd FileType tex setlocal formatoptions+=l tw=80
autocmd FileType markdown setlocal formatoptions+=l tw=80
autocmd FileType text setlocal formatoptions+=l tw=80

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

