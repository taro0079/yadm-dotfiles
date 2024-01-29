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
"set foldmethod=indent
set laststatus=2
set path+=**
set statusline=%F%m%h%w\ %<[ENC=%{&fenc!=''?&fenc:&enc}]\ [FMT=%{&ff}]\[TYPE=%Y]\ %=[POS=%l/%L(%02v)]
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
"" Plug 'vim-denops/denops.vim'
"Plug 'ojroques/vim-oscyank'
"" Plug 'weirongxu/plantuml-previewer.vim'
"" Plug 'tyru/open-browser.vim'
"" Plug 'aklt/plantuml-syntax'
"Plug 'thinca/vim-qfhl'
"Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-repeat'
"Plug 'tpope/vim-endwise'
"Plug 'prabirshrestha/vim-lsp'
"Plug 'mattn/vim-lsp-settings'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
"" Plug 'prabirshrestha/asyncomplete.vim'
"" Plug 'prabirshrestha/asyncomplete-lsp.vim'
"" Plug 'skywind3000/asyncrun.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
"Plug 'sheerun/vim-polyglot'
Plug 'justinmk/vim-sneak'
"Plug 'morhetz/gruvbox'
Plug 'vim-skk/eskk.vim'
"Plug 'taro0079/fd.vim'
Plug 'tpope/vim-fugitive'
"Plug 'airblade/vim-gitgutter'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'lambdalisue/suda.vim'
"Plug 'kana/vim-textobj-user'
"Plug 'osyo-manga/vim-textobj-blockwise'
"Plug 'bronson/vim-trailing-whitespace'
"Plug 'honza/vim-snippets'
"Plug 'easymotion/vim-easymotion'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mattn/ctrlp-matchfuzzy'
"Plug 'garbas/vim-snipmate'
Plug 'phpactor/phpactor', {'for': 'php', 'tag': '*', 'do': 'composer install --no-dev -o'}
"Plug 'github/copilot.vim'
"Plug 'itchyny/vim-parenmatch'
"Plug 'dense-analysis/ale'
"Plug 'MarcWeber/vim-addon-mw-utils'
"Plug 'lifepillar/vim-solarized8'
call plug#end()

" fold settings ---------------------- {{{1
" set foldmethod=manual
"augroup filetype_vim
"  set foldmethod=manual
"  autocmd!
"  autocmd FileType vim setlocal foldmethod=marker
"augroup END

" list settings ---------------------- {{{1
nnoremap <leader>lt :set list!<CR>

" " lsp settings --- {{{1
"let g:lsp_diagnostics_highlights_insert_mode_enabled = 0
"let g:lsp_diagnostics_enabled = 0
"let g:lsp_diagnostics_float_cursor = 1
"let g:lsp_diagnostics_highlights_enabled = 0
"let g:lsp_diagnostics_virtual_text_align = 'after'
" hi DiagnosticError guifg=Red
" hi DiagnosticWarn  guifg=DarkOrange
" hi DiagnosticInfo  guifg=Blue
" hi DiagnosticHint  guifg=Green

"if executable('ag')
"  let g:ackprg = 'ag --vimgrep'
"endif

" vim lsp settings --- {{{1
"function! s:on_lsp_buffer_enabled() abort
"  setlocal omnifunc=lsp#complete
"  setlocal signcolumn=yes
"  nmap <buffer> gd <plug>(lsp-definition)
"  nmap <buffer> gs <plug>(lsp-document-symbol-search)
"  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
"  nmap <buffer> gr <plug>(lsp-references)
"  nmap <buffer> gi <plug>(lsp-implementation)
"  nmap <buffer> gt <plug>(lsp-type-definition)
"  nmap <buffer> <leader>rn <plug>(lsp-rename)
"  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
"  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
"  nmap <buffer> K <plug>(lsp-hover)
"endfunction

" set list
" set listchars=tab:»-,trail:-,eol:¿,extends:»,precedes:«,nbsp:%
" vim color scheme settings --- {{{1
syntax enable
filetype plugin on
"set termguicolors
"set background=dark
" colorscheme gruvbox
"colorscheme solarized8

" ESKK setting ------------------------------- {{{1
let g:eskk#directory        = "~/.config/eskk"
let g:eskk#dictionary       = { 'path': "~/.config/eskk/my_jisyo", 'sorted': 1, 'encoding': 'utf-8',}
let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}

" OSC52 setting --- {{{1
"nmap <leader>c <Plug>OSCYankOperator
"nmap <leader>cc <leader>c_
"vmap <leader>c <Plug>OSCYankVisual

" " vimrc setting --- {{{1
"nnoremap <silent> <leader><CR> :source ~/.vimrc<CR>
"nnoremap <silent> <leader>v :e ~/.vimrc<CR>
" tmux seeting
"let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

"vim easymotion setting ---- {{{1
"xmap ga <Plug>(EasyAlign)
"nmap ga <Plug>(EasyAlign)

" newtrw settings ---- {{{1
let g:netrw_banner       = 0
let g:netrw_browse_split = 3
let g:netrw_altv         = 1
let g:netrw_liststyle    = 3
let g:netrw_list_hide    = netrw_gitignore#Hide()

" lsp settings ---------------------------- {{{1

"augroup lsp_install
"    au!
"    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
"augroup END
" command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

" rspt チケット番号抽出コマンド ---- {{{1
function! RpstTicketNum()
    execute ":%v/^#\\d.\\+/s/.*//g"
    execute ":%s/^#\\(\\d\\+\\)\\|.+$/\\1/g"
    execute ":%s/^\\n//g"
endfunction

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

" coc settings
" May need for Vim (not Neovim) since coc.nvim calculates byte offset by count
" utf-8 byte sequence
set encoding=utf-8
" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
"                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
"
function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
"
"" Use <c-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif
"
"" Use `[g` and `]g` to navigate diagnostics
"" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>
"
function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

"" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')
"
"" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)
"
"" Formatting selected code
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
"
augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying code actions to the selected code block
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)
"
" Remap keys for applying code actions at the cursor position
nmap <leader>ac  <Plug>(coc-codeaction-cursor)
" Remap keys for apply code actions affect whole buffer
nmap <leader>as  <Plug>(coc-codeaction-source)
" Apply the most preferred quickfix action to fix diagnostic on the current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap keys for applying refactor code actions
nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)
"
" Remap <C-f> and <C-b> to scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
" Requires 'textDocument/selectionRange' support of language server
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
"
"
"" php actor
"" 画面を分割して定義元へのジャンプ
"function! DefinitionJumpWithPhpactor()
"    split
"    call phpactor#GotoDefinition()
"endfunction
"
"" useの補完
"nmap <silent><Leader>u      :<C-u>call phpactor#UseAdd()<CR>
"" コンテキストメニューの起動(カーソル下のクラスやメンバに対して実行可能な選択肢を表示してくれます)
"nmap <silent><Leader>mm     :<C-u>call phpactor#ContextMenu()<CR>
"" ナビゲーションメニューの起動(クラスの参照元を列挙したり、他ファイルへのジャンプなど)
"nmap <silent><Leader>nn     :<C-u>call phpactor#Navigate()<CR>
"" カーソル下のクラスやメンバの定義元にジャンプ
"nmap <silent><Leader>o      :<C-u>call phpactor#GotoDefinition()<CR>
"" 編集中のクラスに対し各種の変更を加える(コンストラクタ補完、インタフェース実装など)
"nmap <silent><Leader>tt     :<C-u>call phpactor#Transform()<CR>
"" 新しいクラスを生成する(編集中のファイルに)
"nmap <silent><Leader>cc     :<C-u>call phpactor#ClassNew()<CR>
"" 選択した範囲を変数に抽出する
"nmap <silent><Leader>ee     :<C-u>call phpactor#ExtractExpression(v:false)<CR>
"" 選択した範囲を変数に抽出する
"vmap <silent><Leader>ee     :<C-u>call phpactor#ExtractExpression(v:true)<CR>
"" 選択した範囲を新たなメソッドとして抽出する
"vmap <silent><Leader>em     :<C-u>call phpactor#ExtractMethod()<CR>
"" split → jump
"nmap <silent><C-w><Leader>o :<C-u>call DefinitionJumpWithPhpactor()<CR>
"" カーソル下のクラスや変数の情報を表示する
"" 他のエディタで、マウスカーソルをおいたときに表示されるポップアップなどに相当
"vmap <silent><Leader>hh     :<C-u>call phpactor#Hover()<CR>
