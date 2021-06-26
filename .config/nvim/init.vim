"--------------------------------------
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

