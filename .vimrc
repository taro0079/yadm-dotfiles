" basic settings ---------------------------- {{{1
let mapleader = ','
set title
" set cmdheight=1
" set tags=tags;
" set clipboard&
set clipboard=unnamed,unnamedplus
set showcmd
set ruler
set undofile
set relativenumber
set linebreak
set display+=lastline
set number
set showmatch
set incsearch
set nocompatible
" set signcolumn=yes
set smartcase
set hlsearch
set incsearch
set nobackup
set nowb
set noswapfile
set noundofile
set wildmenu
set backspace=indent,eol,start
set expandtab
set listchars=eol:¿,tab:>.,trail:~,space:¿,nbsp:%
" set list
set tabstop=4
set shiftwidth=4
set showtabline=2
set showcmd
set ai "Auto Indent"
set si "Smart Indent"
set wrap "Wrap lines"
set hidden
set cursorline
set ignorecase
set t_Co=256
" set foldmethod=indent
" set foldlevel=1
" set foldclose=all
set laststatus=2
set path+=**
set statusline=%F%m%h%w\ %<[ENC=%{&fenc!=''?&fenc:&enc}]\ [FMT=%{&ff}]\ [TYPE=%Y]\ %=[CODE=0x%02B]\ [POS=%l/%L(%02v)]
if executable('rg')
    let &grepprg = 'rg --vimgrep --hidden'
    set grepformat=%f:%l:%c:%m
endif

" Plugins ---------------------------- {{{1
call plug#begin()
" Plug 'MattesGroeger/vim-bookmarks'
Plug 'danilo-augusto/vim-afterglow'
" Plug 'taro0079/path_to_clipboard'
Plug 'vim-denops/denops.vim'
" Plug 'morhetz/gruvbox'
Plug 'ojroques/vim-oscyank'
Plug 'thinca/vim-qfhl'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-sqlfmt'
Plug 'mattn/vim-lsp-settings'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'skywind3000/asyncrun.vim'

Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'

Plug 'mattn/emmet-vim'
Plug 'justinmk/vim-sneak'
Plug 'vim-skk/eskk.vim'
Plug 'tomasr/molokai'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'lifepillar/vim-solarized8'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'taro0079/fd.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'eshion/vim-sync'
Plug 'easymotion/vim-easymotion'
" Plug 'prettier/vim-prettier', { 'do': 'yarn install --frozen-lockfile --production'  }
" Plug 'lambdalisue/suda.vim'
" Plug 'kana/vim-textobj-user'
" Plug 'osyo-manga/vim-textobj-blockwise'
Plug 'bronson/vim-trailing-whitespace'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'


call plug#end()

" vim-bookmark setting --- {{{1
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

" FZF settings ---- {{{1
" let g:fzf_layout = { 'window': 'split' }
nmap <C-p> :GFiles<CR>
nmap <leader>ff :Files<CR>
nmap <leader>fq :RG<CR>

" easymotion settings ----------------------{{{1
" map f <Plug>(easymotion-fl)
" map t <Plug>(easymotion-tl)
" map F <Plug>(easymotion-Fl)
" map T <Plug>(easymotion-Tl)
" " suda setting {{{1
" let g:suda_smart_edit = 1

" fold settings ---------------------- {{{1
set foldmethod=manual
augroup filetype_vim
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END

" list settings ---------------------- {{{1
nnoremap <leader>lt :set list!<CR>

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

" set list
" set listchars=tab:»-,trail:-,eol:¿,extends:»,precedes:«,nbsp:%
" vim color scheme settings --- {{{1
syntax enable
filetype plugin on
set termguicolors
set background=dark
colorscheme gruvbox
" colorscheme solarized8

" ESKK setting ------------------------------- {{{1
let g:eskk#directory        = "~/.config/eskk"
let g:eskk#dictionary       = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}
let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}

" OSC52 setting --- {{{1
nmap <leader>c <Plug>OSCYankOperator
nmap <leader>cc <leader>c_
vmap <leader>c <Plug>OSCYankVisual
" toggle term setting === {{{1
" let g:terminal_bufnr = -1

" function! ToggleTerminal()
"   if &buftype == 'terminal'
"     " If the current buffer is a terminal, go back to the previous buffer
"     execute "buffer #"
"     execute "close"
"   else
"     " If the current buffer is not a terminal, try to find the terminal buffer
"     if bufexists(g:terminal_bufnr)
"       " If the terminal buffer exists, switch to it
"       execute 'split'
"       execute "buffer " . g:terminal_bufnr
"       execute "normal i"

"     else
"       " If no terminal buffer exists, create a new one and save its buffer number
"       terminal
"       let g:terminal_bufnr = bufnr('%')
"     endif

"   endif
" endfunction

" nnoremap <silent> <C-t> :call ToggleTerminal()<CR>
" tnoremap <silent> <C-t> <C-\><C-n>:call ToggleTerminal()<CR>


" " vimrc setting --- {{{1
nnoremap <silent> <leader><CR> :source ~/.vimrc<CR>
nnoremap <silent> <leader>v :e ~/.vimrc<CR>
" tmux seeting
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" vim easymotion setting ---- {{{1
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" vim-vsnip setting ---- {{{1
" imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
" smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
" " Jump forward or backward
" imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
" smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
" imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
" smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" newtrw settings ---- {{{1
let g:netrw_banner=0
let g:netrw_browse_split=3
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()

" lsp settings ---------------------------- {{{1
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gr <plug>(lsp-reference)
endfunction

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

" rspt チケット番号抽出コマンド ---- {{{1
function! RpstTicketNum()
    execute ":%v/^#\\d.\\+/s/.*//g"
    execute ":%s/^#\\(\\d\\+\\)\\|.+$/\\1/g"
    execute ":%s/^\\n//g"
endfunction

" denops setting --- {{{1
set runtimepath^=~/dev/denops-tutorial
" let g:denops#debug = 1

let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
" If you want :UltiSnipsEdit to split your window.
" let g:UltiSnipsEditSplit="vertical"
