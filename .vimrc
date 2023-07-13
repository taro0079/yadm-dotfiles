" basic settings ---------------------------- {{{1
let mapleader = ','
set title
" set cmdheight=1
set laststatus=3
" set tags=tags;
" set clipboard&
set clipboard=unnamed,unnamedplus
set showcmd
set ruler
set undofile
set cursorline
set relativenumber
set linebreak
set display+=lastline
set number
set showmatch
set incsearch
set nocompatible
set signcolumn=yes
set smartcase
set hlsearch
set incsearch
set nobackup
set nowb
set noswapfile
set noundofile
set wildmenu
set showtabline=2
set showcmd
set ai "Auto Indent"
set si "Smart Indent"
set wrap "Wrap lines"

call plug#begin()
" Plug 'machakann/vim-sandwich'
" Plug 'MattesGroeger/vim-bookmarks'
Plug 'nordtheme/vim'
Plug 'taro0079/path_to_clipboard'
" Plug 'axvr/photon.vim'
Plug 'github/copilot.vim'
Plug 'morhetz/gruvbox'
Plug 'prabirshrestha/async.vim'
Plug 'thinca/vim-qfhl'
Plug 'junegunn/vim-easy-align'
" Plug 'EinfachToll/DidYouMean'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'soramugi/auto-ctags.vim'
Plug 'sheerun/vim-polyglot'
" Plug 'dense-analysis/ale'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
" Plug 'thomasfaingnaert/vim-lsp-snippets'
" Plug 'thomasfaingnaert/vim-lsp-ultisnips'
" Plug 'majutsushi/tagbar'
Plug 'justinmk/vim-sneak'
Plug 'liuchengxu/vista.vim'
Plug 'vim-skk/eskk.vim'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" Plug 'fatih/vim-go'
Plug 'easymotion/vim-easymotion'
" Plug 'prettier/vim-prettier', { 'do': 'yarn install --frozen-lockfile --production'  }
" Plug 'kana/vim-textobj-user'
" Plug 'osyo-manga/vim-textobj-blockwise'
" fern
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'lambdalisue/nerdfont.vim'
Plug 'mileszs/ack.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'lambdalisue/suda.vim'
" Plug 'tc50cal/vim-terminal'
" Plug 'bronson/vim-trailing-whitespace'
Plug 'ojroques/vim-oscyank'

call plug#end()

  nmap <buffer> gd <plug>(lsp-definition)
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
endfunction

" " vim-bookmark setting
" nmap <Leader><Leader> <Plug>BookmarkToggle
" nmap <Leader>i <Plug>BookmarkAnnotate
" nmap <Leader>a <Plug>BookmarkShowAll
" nmap <Leader>j <Plug>BookmarkNext
" nmap <Leader>k <Plug>BookmarkPrev
" nmap <Leader>c <Plug>BookmarkClear
" nmap <Leader>x <Plug>BookmarkClearAll
" nmap <Leader>kk <Plug>BookmarkMoveUp
" nmap <Leader>jj <Plug>BookmarkMoveDown
" nmap <Leader>g <Plug>BookmarkMoveToLine

" " FZF settings
" nmap <C-p> :GFiles<CR>
" nmap <leader>ff :Files<CR>
" nmap <leader>fq :RG<CR>

" " eeasymotion settings ----------------------{{{1
" map f <Plug>(easymotion-fl)
" map t <Plug>(easymotion-tl)
" map F <Plug>(easymotion-Fl)
" map T <Plug>(easymotion-Tl)
