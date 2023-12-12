" basic settings ---------------------------- {{{1
set title
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
set display+=lastline
set number
set showmatch
set incsearch
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
set listchars=tab:»-,trail:-,eol:↲,extends:»,precedes:«,nbsp:%
set list
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
set foldmethod=indent
set laststatus=2
set path+=**
set statusline=%F%m%h%w\ %<[ENC=%{&fenc!=''?&fenc:&enc}]\ [FMT=%{&ff}]\[TYPE=%Y]\ %=[POS=%l/%L(%02v)]\ [%{fugitive#statusline()}]
if executable('rg')
    let &grepprg = 'rg --vimgrep --hidden'
    set grepformat=%f:%l:%c:%m
endif
" Plugins ---------------------------- {{{1
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    \ >/dev/null 2>&1
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin()
Plug 'taro0079/path_to_clipboard'
" Plug 'vim-denops/denops.vim'
Plug 'ojroques/vim-oscyank'
" Plug 'weirongxu/plantuml-previewer.vim'
" Plug 'tyru/open-browser.vim'
" Plug 'aklt/plantuml-syntax'
Plug 'thinca/vim-qfhl'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
" Plug 'prabirshrestha/asyncomplete.vim'
" Plug 'prabirshrestha/asyncomplete-lsp.vim'
" Plug 'skywind3000/asyncrun.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
Plug 'sheerun/vim-polyglot'
Plug 'justinmk/vim-sneak'
Plug 'morhetz/gruvbox'
Plug 'vim-skk/eskk.vim'
Plug 'taro0079/fd.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" Plug 'eshion/vim-sync'
Plug 'lambdalisue/suda.vim'
Plug 'kana/vim-textobj-user'
Plug 'osyo-manga/vim-textobj-blockwise'
Plug 'bronson/vim-trailing-whitespace'
Plug 'honza/vim-snippets'
Plug 'easymotion/vim-easymotion'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mattn/ctrlp-matchfuzzy'
Plug 'garbas/vim-snipmate'
Plug 'phpactor/phpactor', {'for': 'php', 'tag': '*', 'do': 'composer install --no-dev -o'}
Plug 'github/copilot.vim'
Plug 'itchyny/vim-parenmatch'
Plug 'dense-analysis/ale'
Plug 'Shougo/ddc-source-around'
Plug 'shun/ddc-source-vim-lsp'
Plug 'Shougo/ddc.vim'
Plug 'Shougo/pum.vim'
Plug 'vim-denops/denops.vim'
Plug 'Shougo/ddc-around'
Plug 'Shougo/ddc-filter-matcher_head'
Plug 'Shougo/ddc-ui-native'
Plug 'tani/ddc-fuzzy'
Plug 'Shougo/ddc-source-cmdline'
Plug 'Shougo/ddc-source-cmdline-history'
Plug 'Shougo/ddc-ui-native'
Plug 'Shougo/ddc-ui-pum'
Plug 'Shougo/ddc-source-rg'
Plug 'MarcWeber/vim-addon-mw-utils'
call plug#end()

" fold settings ---------------------- {{{1
" set foldmethod=manual
augroup filetype_vim
  set foldmethod=manual
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END

" list settings ---------------------- {{{1
nnoremap <leader>lt :set list!<CR>

" " lsp settings --- {{{1
let g:lsp_diagnostics_highlights_insert_mode_enabled = 0
let g:lsp_diagnostics_enabled = 0
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_diagnostics_highlights_enabled = 0
let g:lsp_diagnostics_virtual_text_align = 'after'
" hi DiagnosticError guifg=Red
" hi DiagnosticWarn  guifg=DarkOrange
" hi DiagnosticInfo  guifg=Blue
" hi DiagnosticHint  guifg=Green

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

" " vimrc setting --- {{{1
nnoremap <silent> <leader><CR> :source ~/.vimrc<CR>
nnoremap <silent> <leader>v :e ~/.vimrc<CR>
" tmux seeting
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" vim easymotion setting ---- {{{1
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" newtrw settings ---- {{{1
let g:netrw_banner       = 0
let g:netrw_browse_split = 3
let g:netrw_altv         = 1
let g:netrw_liststyle    = 3
let g:netrw_list_hide    = netrw_gitignore#Hide()

" lsp settings ---------------------------- {{{1

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
" command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

" rspt チケット番号抽出コマンド ---- {{{1
function! RpstTicketNum()
    execute ":%v/^#\\d.\\+/s/.*//g"
    execute ":%s/^#\\(\\d\\+\\)\\|.+$/\\1/g"
    execute ":%s/^\\n//g"
endfunction

" denops setting --- {{{1
set runtimepath^=~/dev/denops-tutorial
" let g:denops#debug = 1

" myfd --- {{{1
nmap <leader>fd :MyFd<cr>

" CtrlP settings --- {{{1
let g:ctrlp_match_func = {'match': 'ctrlp_matchfuzzy#matcher'}

" snipmate setting --- {{{1
let g:snipMate = { 'snippet_version' : 1 }
imap <C-k> <Plug>snipMateNextOrTrigger


function! ProfileCursorMove() abort
  let profile_file = expand('~/log/vim-profile.log')
  if filereadable(profile_file)
    call delete(profile_file)
  endif

  normal! gg
  normal! zR

  execute 'profile start ' . profile_file
  profile func *
  profile file *

  augroup ProfileCursorMove
    autocmd!
    autocmd CursorHold <buffer> profile pause | q
  augroup END

  for i in range(2000)
    call feedkeys('j')
  endfor
endfunction

let g:loaded_matchparen = 1
if executable('rg')
  let g:ctrlp_use_caching = 0
  "let g:ctrlp_user_command = 'cd %s && rg "" -i -r --no-color -l ./**/*'
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
endif
" 正規表現のマッチングエンジンを変更
" set regexpengine=1
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_sign_error = '✘'
let g:ale_sign_warning = '⚠'
let g:ale_lint_on_text_changed = 'never'
let g:ale_fix_on_save = 1
let g:ale_fixers = {'php': ['php_cs_fixer']}


" ddc setting
call ddc#custom#patch_global(#{
      \  ui: 'pum',
      \  autoCompleteEvents: ['InsertEnter', 'TextChangedI', 'TextChangedP', 'CmdlineChanged'],
      \  cmdlineSources: {
      \    ':': ['cmdline', 'around', 'cmdline-history']
      \  }
      \})
call ddc#custom#patch_global('sources', ['around', 'vim-lsp', 'cmdline', 'cmdline-history', 'rg'])
call ddc#custom#patch_global('sourceOptions', #{
            \ _: #{
            \   matchers: ['matcher_fuzzy'],
            \   sorters: ['sorter_fuzzy'],
            \   converters: ['converter_fuzzy']
            \ },
            \ around: #{ mark: 'A' },
            \ vim-lsp: #{
            \   matchers: ['matcher_fuzzy'],
            \   forceCompletionPattern: '\.\w*|:\w*|->\w*',
            \   mark: 'lsp',
            \ },
            \ cmdline: #{
            \   mark: 'cmdline',
            \ },
            \ cmdline-history: #{
            \  mark: 'cmdline-history',
            \ },
            \ rg: #{
            \   mark: 'rg',
            \   minAutoCompleteLength: 3,
            \ }
            \ })
call ddc#custom#patch_global('sourceParams', #{
      \ around: #{ maxsize: 500 }
      \})

" inoremap <silent><expr> <TAB>
"       \ pum#visible() ? '<Cmd>call pum#map#insert_relative(+1)<CR>' :
"       \ (col('.') <= 1 <Bar><Bar> getline('.')[col('.') - 2] =~# '\s') ?
"       \ '<TAB>' : ddc#map#manual_complete()
" inoremap <S-Tab> <Cmd>call pum#map#insert_relative(-1)<CR>
inoremap <C-n>   <Cmd>call pum#map#insert_relative(+1)<CR>
inoremap <C-p>   <Cmd>call pum#map#insert_relative(-1)<CR>
inoremap <C-y>   <Cmd>call pum#map#confirm()<CR>
inoremap <C-e>   <Cmd>call pum#map#cancel()<CR>

nnoremap :       <Cmd>call CommandlinePre()<CR>:

function! CommandlinePre() abort
    cnoremap <Tab>   <Cmd>call pum#map#insert_relative(+1)<CR>
    cnoremap <S-Tab> <Cmd>call pum#map#insert_relative(-1)<CR>
    cnoremap <C-n>   <Cmd>call pum#map#insert_relative(+1)<CR>
    cnoremap <C-p>   <Cmd>call pum#map#insert_relative(-1)<CR>
    cnoremap <C-y>   <Cmd>call pum#map#confirm()<CR>
    cnoremap <C-e>   <Cmd>call pum#map#cancel()<CR>

    autocmd User DDCCmdlineLeave ++once call CommandlinePost()

    " Enable command line completion for the buffer
    call ddc#enable_cmdline_completion()
endfunction
function! CommandlinePost() abort
    silent! cunmap <Tab>
    silent! cunmap <S-Tab>
    silent! cunmap <C-n>
    silent! cunmap <C-p>
    silent! cunmap <C-y>
    silent! cunmap <C-e>
endfunction
call ddc#enable()

