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
set hidden

" Plugins ---------------------------- {{{1
call plug#begin()
" Plug 'machakann/vim-sandwich'
Plug 'MattesGroeger/vim-bookmarks'
" Plug 'nordtheme/vim'
Plug 'tomasr/molokai'
Plug 'joshdick/onedark.vim'
Plug 'taro0079/path_to_clipboard'
Plug 'github/copilot.vim'
Plug 'morhetz/gruvbox'
Plug 'ojroques/vim-oscyank'
Plug 'prabirshrestha/async.vim'
Plug 'thinca/vim-qfhl'
Plug 'junegunn/vim-easy-align'
Plug 'EinfachToll/DidYouMean'
Plug 'christoomey/vim-tmux-navigator'
Plug 'tpope/vim-repeat'
Plug 'dracula/vim'
Plug 'tpope/vim-endwise'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'soramugi/auto-ctags.vim'
Plug 'sheerun/vim-polyglot'
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'
Plug 'mattn/emmet-vim'
" Plug 'majutsushi/tagbar'
Plug 'patstockwell/vim-monokai-tasty'
Plug 'justinmk/vim-sneak'
Plug 'liuchengxu/vista.vim'
Plug 'vim-skk/eskk.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'easymotion/vim-easymotion'
Plug 'prettier/vim-prettier', { 'do': 'yarn install --frozen-lockfile --production'  }
" fern
Plug 'lambdalisue/fern.vim'
Plug 'lambdalisue/fern-git-status.vim'
Plug 'lambdalisue/nerdfont.vim'
Plug 'mileszs/ack.vim'
Plug 'lambdalisue/fern-renderer-nerdfont.vim'
Plug 'lambdalisue/glyph-palette.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'lambdalisue/suda.vim'
Plug 'kana/vim-textobj-user'
Plug 'osyo-manga/vim-textobj-blockwise'
Plug 'tc50cal/vim-terminal'
Plug 'bronson/vim-trailing-whitespace'
Plug 'ojroques/vim-oscyank'

call plug#end()

" vim-bookmark setting --- {{{1
nmap <Leader><Leader> <Plug>BookmarkToggle
nmap <Leader>i <Plug>BookmarkAnnotate
nmap <Leader>a <Plug>BookmarkShowAll
nmap <Leader>j <Plug>BookmarkNext
nmap <Leader>k <Plug>BookmarkPrev
nmap <Leader>c <Plug>BookmarkClear
nmap <Leader>x <Plug>BookmarkClearAll
nmap <Leader>kk <Plug>BookmarkMoveUp
nmap <Leader>jj <Plug>BookmarkMoveDown
nmap <Leader>g <Plug>BookmarkMoveToLine

" FZF settings ---- {{{1
nmap <C-p> :GFiles<CR>
nmap <leader>ff :Files<CR>
nmap <leader>fq :RG<CR>

" easymotion settings ----------------------{{{1
map f <Plug>(easymotion-fl)
map t <Plug>(easymotion-tl)
map F <Plug>(easymotion-Fl)
map T <Plug>(easymotion-Tl)
" suda setting {{{1
let g:suda_smart_edit = 1

" fold settings ---------------------- {{{1
set foldmethod=manual
augroup filetype_vim
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END

" lsp settings --- {{{1
let g:lsp_diagnostics_highlights_insert_mode_enabled = 1
let g:lsp_diagnostics_enabled = 1
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_highlights_enabled = 1
let g:lsp_diagnostics_virtual_text_align = 'after'
hi DiagnosticError guifg=Red
hi DiagnosticWarn  guifg=DarkOrange
hi DiagnosticInfo  guifg=Blue
hi DiagnosticHint  guifg=Green

if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" vim lsp settings --- {{{1
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  nmap <buffer> gd <plug>(lsp-definition)
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

set list
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
" vim color scheme settings --- {{{1
syntax on
set termguicolors
let g:vim_monokai_tasty_italic = 1
colorscheme vim-monokai-tasty


" ESKK setting ------------------------------- {{{1
let g:eskk#directory        = "~/.config/eskk"
let g:eskk#dictionary       = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}
let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}
" OSC52 setting --- {{{1
nmap <leader>c <Plug>OSCYankOperator
nmap <leader>cc <leader>c_
vmap <leader>c <Plug>OSCYankVisual

" UltiSnips setting --- {{{1

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" toggle term setting === {{{1
let g:terminal_bufnr = -1

function! ToggleTerminal()
  if &buftype == 'terminal'
    " If the current buffer is a terminal, go back to the previous buffer
    execute "buffer #"
    execute "close"
  else
    " If the current buffer is not a terminal, try to find the terminal buffer
    if bufexists(g:terminal_bufnr)
      " If the terminal buffer exists, switch to it
      execute 'split'
      execute "buffer " . g:terminal_bufnr
      execute "normal i"

    else
      " If no terminal buffer exists, create a new one and save its buffer number
      terminal
      let g:terminal_bufnr = bufnr('%')
    endif

  endif
endfunction

nnoremap <silent> <C-t> :call ToggleTerminal()<CR>
tnoremap <silent> <C-t> <C-\><C-n>:call ToggleTerminal()<CR>


" vimrc setting --- {{{1
nnoremap <silent> <leader><CR> :source ~/.vimrc<CR>
nnoremap <silent> <leader>v :e ~/.vimrc<CR>


