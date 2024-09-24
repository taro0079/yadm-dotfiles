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
" set signcolumn=yes
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
"set foldmethod=indent
set laststatus=2
set path+=**
" Use English interface.
language message C
" set statusline=%F%m%h%w\ %<[ENC=%{&fenc!=''?&fenc:&enc}]\ [FMT=%{&ff}]\[TYPE=%Y]\ %=[POS=%l/%L(%02v)]
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
Plug 'vim-denops/denops.vim'
Plug 'ojroques/vim-oscyank'
Plug 'Shougo/ddc.vim'
Plug 'Shougo/ddc-ui-native'
Plug 'Shougo/ddc-source-around'
Plug 'Shougo/ddc-filter-matcher_head'
Plug 'Shougo/ddc-filter-sorter_rank'
Plug 'shun/ddc-source-vim-lsp'
Plug 'Shougo/ddu.vim'
Plug 'Shougo/ddu-ui-ff'
Plug 'Shougo/ddu-kind-file'
Plug 'Shougo/ddu-source-file'
Plug 'Shougo/ddu-source-file_rec'
Plug 'Shougo/ddu-source-register'
Plug 'Shougo/ddu-commands.vim'
Plug 'matsui54/ddu-source-file_external'
Plug 'tpope/vim-dadbod'
Plug 'shun/ddu-source-buffer'
Plug 'Shougo/ddu-filter-matcher_substring'
Plug 'junegunn/vim-easy-align'
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
Plug 'vim-skk/eskk.vim'
" Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-gitgutter'
Plug 'lambdalisue/suda.vim'
Plug 'kana/vim-textobj-user'
Plug 'osyo-manga/vim-textobj-blockwise'
Plug 'dracula/vim'
Plug 'bronson/vim-trailing-whitespace'
Plug 'easymotion/vim-easymotion'
Plug 'phpactor/phpactor', {'for': 'php', 'tag': '*', 'do': 'composer install --no-dev -o'}
Plug 'vim-test/vim-test'
" Plug 'dense-analysis/ale'
Plug 'lifepillar/vim-solarized8'
Plug 'thinca/vim-quickrun'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'
call plug#end()

" ESKK setting ------------------------------- {{{1
let g:eskk#directory        = "~/.config/eskk"
let g:eskk#dictionary       = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}
let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}
imap <C-j> <Plug>(eskk:toggle)

" Color settings ------------------------------- {{{1
syntax enable
filetype plugin on
set termguicolors
set background=dark
colorscheme dracula

" fold settings ---------------------- {{{1
" set foldmethod=manual
"augroup filetype_vim
"  set foldmethod=manual
"  autocmd!
"  autocmd FileType vim setlocal foldmethod=marker
"augroup END

" list settings ---------------------- {{{1
nnoremap <leader>lt :set list!<CR>

" ddc setings ---------------------- {{{1
call ddc#custom#patch_global('ui', 'native')
call ddc#custom#patch_global('sources', ['vim-lsp', 'around'])
call ddc#custom#patch_global('sourceOptions', {
      \ '_': {
      \    'matchers': ['matcher_head'],
      \    'sorters': ['sorter_rank'],
      \    'minAutoCompleteLength': 1
      \  },
      \ 'around': {
      \   'mark': 'A',
      \ },
      \ 'vim-lsp': {
      \    'matchers': ['matcher_head'],
      \    'mark': 'lsp',
      \    'maxItems': 15,
      \ },
      \})
call ddc#enable()

" ddu settings ---------------------- {{{1
call ddu#custom#patch_global({
      \  'ui': 'ff',
      \  'sources': [{'name':'register'},
      \    {
      \      'name': 'file_rec',
      \      'params': {},
      \    }],
      \  'sourceParams': {
      \    'file_external': {
      \      'cmd': ['git', 'ls-files']
      \    },
      \    'file_rec': {
      \        'ignoredDirectories': ['.git', 'node_modules', 'vendor', '.next']
      \    },
      \  },
      \  'sourceOptions': {
      \    '_': {
      \      'matchers': ["matcher_substring"],
      \      'ignoreCase': 'true',
      \    },
      \  },
      \  'filterParams': {
      \    'matcher_substring': {
      \      'highlightMatched': 'Title'
      \    }
      \  },
      \  'kindOptions': {
      \    'file_rec': {
      \      'defaultAction': 'open',
      \    },
      \    'file_external': {
      \      'defaultAction': 'open',
      \    }
      \  }
      \})

" ddu key setting
autocmd FileType ddu-ff call s:ddu_my_settings()
function! s:ddu_my_settings() abort
    nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#do_action('itemAction',
                \ {'name': 'open', 'params':{}})<CR>
  nnoremap <buffer><silent> <Space> <Cmd>call ddu#ui#do_action('toggleSelectItem')<CR>
  nnoremap <buffer><silent> i <Cmd>call ddu#ui#do_action('openFilterWindow')<CR>
  nnoremap <buffer><silent> q <Cmd>call ddu#ui#do_action('quit')<CR>
endfunction

autocmd FileType ddu-ff-filter call s:ddu_filter_my_settings()
function! s:ddu_filter_my_settings() abort
  nnoremap <buffer> <CR> <Cmd>call ddu#ui#do_action('itemAction')<CR>
  nnoremap <buffer><silent> q <Cmd>call ddu#ui#do_action('quit')<CR>
  inoremap <buffer> <CR> <Cmd>call ddu#ui#do_action('itemAction')<CR>
  inoremap <buffer> <C-n> <Cmd>call ddu#ui#ff#execute("call cursor(line('.')+1,0)")<CR>
  inoremap <buffer> <C-p> <Cmd>call ddu#ui#ff#execute("call cursor(line('.')-1,0)")<CR>
endfunction

" ddu keymapping
nnoremap <SID>[ug] <Nop>
nmap ,u <SID>[ug]

nnoremap <silent> <SID>[ug]m :<C-u>Ddu mr<CR>
nnoremap <silent> <SID>[ug]b :<C-u>Ddu buffer<CR>
nnoremap <silent> <SID>[ug]r :<C-u>Ddu register<CR>
nnoremap <silent> <SID>[ug]n :<C-u>Ddu file -source-param-new -source-option-volatile<CR>
nnoremap <silent> <SID>[ug]f :<C-u>Ddu file_rec<CR>
nnoremap <silent> <SID>[ug]p <Cmd>call ddu#start({
            \  'name': 'file_external',
            \  'sources': [{'name': 'file_external'}]
            \ })<CR>

" lsp settings --- {{{1
" let g:lsp_settings = {
" \  'typeprof': {'disabled': 1},
" \}
" let g:lsp_diagnostics_highlights_insert_mode_enabled = 0
" let g:lsp_diagnostics_enabled = 0
" let g:lsp_diagnostics_float_cursor = 1
" let g:lsp_diagnostics_highlights_enabled = 0
" let g:lsp_diagnostics_virtual_text_align = 'after'
"  hi DiagnosticError guifg=Red
"  hi DiagnosticWarn  guifg=DarkOrange
"  hi DiagnosticInfo  guifg=Blue
"  hi DiagnosticHint  guifg=Green

"if executable('ag')
"  let g:ackprg = 'ag --vimgrep'
"endif

" vim lsp settings --- {{{1
" function! s:on_lsp_buffer_enabled() abort
"   setlocal omnifunc=lsp#complete
"   setlocal signcolumn=yes
"   nmap <buffer> gd <plug>(lsp-definition)
"   nmap <buffer> gs <plug>(lsp-document-symbol-search)
"   nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
"   nmap <buffer> gr <plug>(lsp-references)
"   nmap <buffer> gi <plug>(lsp-implementation)
"   nmap <buffer> gt <plug>(lsp-type-definition)
"   nmap <buffer> <leader>rn <plug>(lsp-rename)
"   nmap <buffer> [g <plug>(lsp-previous-diagnostic)
"   nmap <buffer> ]g <plug>(lsp-next-diagnostic)
"   nmap <buffer> K <plug>(lsp-hover)
" endfunction

" set list
" set listchars=tab:»-,trail:-,eol:¿,extends:»,precedes:«,nbsp:%
" vim color scheme settings --- {{{1

" OSC52 setting --- {{{1
"nmap <leader>c <Plug>OSCYankOperator
"nmap <leader>cc <leader>c_
"vmap <leader>c <Plug>OSCYankVisual

" keymaps
" vimrc setting --- {{{1
nnoremap <silent> <leader><CR> :source ~/.vimrc<CR>
nnoremap <silent> <leader>v :e ~/.vimrc<CR>

"vim EasyAlign setting ---- {{{1
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" phpactor setting
autocmd FileType php setlocal omnifunc=phpactor#Complete
" useの補完
nmap <silent><Leader>u      :<C-u>call phpactor#UseAdd()<CR>
" コンテキストメニューの起動(カーソル下のクラスやメンバに対して実行可能な選択肢を表示してくれます)
nmap <silent><Leader>mm     :<C-u>call phpactor#ContextMenu()<CR>
" ナビゲーションメニューの起動(クラスの参照元を列挙したり、他ファイルへのジャンプなど)
nmap <silent><Leader>nn     :<C-u>call phpactor#Navigate()<CR>
" カーソル下のクラスやメンバの定義元にジャンプ
nmap <silent><Leader>o      :<C-u>call phpactor#GotoDefinition()<CR>
" 編集中のクラスに対し各種の変更を加える(コンストラクタ補完、インタフェース実装など)
nmap <silent><Leader>tt     :<C-u>call phpactor#Transform()<CR>
" 新しいクラスを生成する(編集中のファイルに)
nmap <silent><Leader>cc     :<C-u>call phpactor#ClassNew()<CR>
" 選択した範囲を変数に抽出する
nmap <silent><Leader>ee     :<C-u>call phpactor#ExtractExpression(v:false)<CR>
" 選択した範囲を変数に抽出する
vmap <silent><Leader>ee     :<C-u>call phpactor#ExtractExpression(v:true)<CR>
" 選択した範囲を新たなメソッドとして抽出する
vmap <silent><Leader>em     :<C-u>call phpactor#ExtractMethod()<CR>
" split → jump
nmap <silent><C-w><Leader>o :<C-u>call DefinitionJumpWithPhpactor()<CR>
" カーソル下のクラスや変数の情報を表示する
" 他のエディタで、マウスカーソルをおいたときに表示されるポップアップなどに相当
vmap <silent><Leader>hh     :<C-u>call phpactor#Hover()<CR>

" vim-vsnip -- {{1
" Expand
imap <expr> <C-d>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-d>'
smap <expr> <C-d>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-d>'

" Expand or jump
imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

" Jump forward or backward
imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
" See https://github.com/hrsh7th/vim-vsnip/pull/50
nmap        s   <Plug>(vsnip-select-text)
xmap        s   <Plug>(vsnip-select-text)
nmap        X   <Plug>(vsnip-cut-text)
xmap        X   <Plug>(vsnip-cut-text)

nmap <leader>q :QuickRun<cr>

" asynccomplete settings --- {{1
" ddcを利用するため一旦以下は無効化
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" inoremap <expr> <cr> pumvisible() ? asyncomplete#close_popup() : "\<cr>"

" newtrw settings ---- {{{1
let g:netrw_banner       = 0
let g:netrw_browse_split = 0
let g:netrw_altv         = 1
let g:netrw_liststyle    = 3
let g:netrw_list_hide    = netrw_gitignore#Hide()

" tmux seeting
"let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" lsp settings ---------------------------- {{{1

"augroup lsp_install
"    au!
"    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
"augroup END
" command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

" my functions
"
"rspt チケット番号抽出コマンド ---- {{{1
"
" function! RpstTicketNum()
"     execute ":%v/^#\\d.\\+/s/.*//g"
"     execute ":%s/^#\\(\\d\\+\\)\\|.+$/\\1/g"
"     execute ":%s/^\\n//g"
" endfunction
" command! RpstTicketNum call RpstTicketNum()
"
" 選択範囲のテキストを取得するコマンド
" 範囲の終端は必ず行末になるように設定してある
function! GetRegionText()
    let start = getpos("v")
    let end = getpos("'>")
    return getregion(start, end)
endfunction

" rpst-symfonyのクラス郡を生成するコマンド
function! OmsGenerator()
    let current_file_path =  expand('%')
    let output = system(printf("ruby ~/dev/oms-create-rb/test.rb %s", current_file_path))
    let lines =  split(output, '\n')
    " 最初にバッファの全ての行を削除しておく
    execute '%delete _'
    let current_line = 0
    for line in lines
        call append(current_line, line)
        let current_line += 1
    endfor
    " call setline(1, output)
endfunction
command! OmsForever call OmsGenerator()

" コミットログからチケット番号を自動抽出してくれるコマンド v1
function! ExtractTicketNumbers()
    " バッファ全体の行についてリストとして格納
    let lines = getline(1, '$')
    let ticket_numbers = []

    let pattern = '^#\(\d\+\)'

    for line in lines
        let matches = matchlist(line, pattern)

        if !empty(matches)
            call add(ticket_numbers, matches[1])
        endif
    endfor

    echo ticket_numbers
    return ticket_numbers
endfunction

command! TicketExtract call ExtractTicketNumbers()

function! ReleaseWithN()
    let tickets = join(ExtractTicketNumbers(), ',')
    let command_path = expand('~/dev/make_slack_post/make_slack_post.php')
    let command = printf('php %s %s', command_path, tickets)
    call ExternalCommandOutputToBuffer(command)
endfunction

function! IloveReleaseCommand()
    let pr_number = input("What is target PR number ? > ")
    let command_path = expand('~/dev/make_slack_post/fetch_pr.rb')
    let command = printf('ruby %s %s', command_path, pr_number)
    call ExternalCommandOutputToBuffer(command)
endfunction

function ExternalCommandOutputToBuffer(command)
    let output = system(a:command)
    let lines =  split(output, '\n')
    " 最初にバッファの全ての行を削除しておく
    execute '%delete _'
    let current_line = 0
    for line in lines
        call append(current_line, line)
        let current_line += 1
    endfor
endfunction

command! IloveRelease call IloveReleaseCommand()

" denops setting --- {{{1
"set runtimepath^=~/dev/denops-tutorial
" let g:denops#debug = 1

" myfd --- {{{1
"nmap <leader>fd :MyFd<cr>

" CtrlP settings --- {{{1
"let g:ctrlp_match_func = {'match': 'ctrlp_matchfuzzy#matcher'}

" snipmate setting --- {{{1
"let g:snipMate = { 'snippet_version' : 1 }
"imap <C-k> <Plug>snipMateNextOrTrigger


"function! ProfileCursorMove() abort
"  let profile_file = expand('~/log/vim-profile.log')
"  if filereadable(profile_file)
"    call delete(profile_file)
"  endif
"
"  normal! gg
"  normal! zR
"
"  execute 'profile start ' . profile_file
"  profile func *
"  profile file *
"
"  augroup ProfileCursorMove
"    autocmd!
"    autocmd CursorHold <buffer> profile pause | q
"  augroup END
"
"  for i in range(2000)
"    call feedkeys('j')
"  endfor
"endfunction

"let g:loaded_matchparen = 1
"if executable('rg')
"  let g:ctrlp_use_caching = 0
"  "let g:ctrlp_user_command = 'cd %s && rg "" -i -r --no-color -l ./**/*'
"  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
"endif
" 正規表現のマッチングエンジンを変更
" set regexpengine=1
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
"let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
"let g:ale_sign_error = '✘'
"let g:ale_sign_warning = '⚠'
"let g:ale_lint_on_text_changed = 'never'
"let g:ale_fix_on_save = 1
"let g:ale_fixers = {'php': ['php_cs_fixer']}

"" php actor
" 画面を分割して定義元へのジャンプ
function! DefinitionJumpWithPhpactor()
    split
    call phpactor#GotoDefinition()
endfunction

" vim-test --- {{1
let test#php#phpunit#executable = 'phpunit' " テストランナーをphpunitに変更

