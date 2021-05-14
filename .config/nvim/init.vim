" vim: set foldmethod=marker:

syntax on

"""" Plugins {{{

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Convenience

Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'kana/vim-submode'
Plug 'raimondi/delimitmate'
Plug 'simnalamburt/vim-mundo'
Plug 'liuchengxu/vim-which-key'

" Code Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" c/c++
Plug 'Maxattax97/coc-ccls', {'do': 'yarn install --frozen-lockfile'}
" rust
Plug 'neoclide/coc-rls', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tsserver', {'do': 'yarn install --frozen-lockfile'}
" python
Plug 'fannheyward/coc-pyright', {'do': 'yarn install --frozen-lockfile'}
" ruby
Plug 'neoclide/coc-solargraph', {'do': 'yarn install --frozen-lockfile'}
" bash/sh
Plug 'josa42/coc-sh', {'do': 'yarn install --frozen-lockfile'}

Plug 'neoclide/coc-html', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-css', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-json', {'do': 'yarn install --frozen-lockfile'}


" Git
Plug 'tpope/vim-fugitive'
Plug 'sodapopcan/vim-twiggy'
Plug 'airblade/vim-gitgutter'

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Languages
Plug 'habamax/vim-godot'

" Colors
Plug 'morhetz/gruvbox'

call plug#end()

" }}}

"""" Basic settings {{{

set nocompatible
set tabstop=4
set shiftwidth=4
set softtabstop=0 expandtab
set diffopt+=vertical
set completeopt=menuone
set timeoutlen=300
set termguicolors
set guifont=Source\ Code\ Pro:h12

" Persistent undo
set undofile
set undodir=~/.vim/undo

colorscheme gruvbox

highlight TabLineSel guifg=#ebdbb2 guibg=#665c54

" Custom Functions {{{
set number
set rnu
function! NumberToggle()
  if(&number == 1)
    set nornu
    set nonumber
  else
    set number
    set rnu
  endif
endfunc
function! RelNumberToggle()
  if(&relativenumber == 1)
    set nornu
  else
    set rnu
  endif
endfunc

function! ConfirmCmd(prompt, cmd)
    if (confirm(a:prompt, "&Yes\n&No") == 1)
        execute a:cmd
    endif
endfunc

" }}}

"" Key bindings {{{

let mapleader="\<SPACE>"

" inoremap <silent> <C-j> <C-n>
" inoremap <silent> <C-k> <C-p>

nnoremap <silent> <leader>   :<c-u>WhichKey '<Space>'<cr>

" Leader key shortcuts are configured to roughly resemble spacemacs.

nnoremap <leader>nn :call NumberToggle()<cr>
nnoremap <leader>nr :call RelNumberToggle()<cr>

let g:which_key_map = {}

let g:which_key_map.w = { 'name' : '+windows',
            \ 's': 'horizontal split', 'v': 'vertical split',
            \ 'c': 'close window',
            \ '<': 'resize horizontal', '>': 'resize horizontal' }
nnoremap <leader>w <C-w>

nnoremap <leader>sc :nohl<cr>
nnoremap <leader>so :syntax on<cr>

let g:which_key_map.f = { 'name' : '+files',
            \ 'f': 'find git-tracked file', 'F': 'find file', 'b': 'browse files',
            \ 'o': 'find previous file', 'g': 'grep files',
            \ 'r': 'rename current file', 'd': 'delete current file', 'D': 'delete current file and buffer',
            \ 's': 'save', 'S': 'sudo save'
            \ }
nnoremap <leader>ff :Telescope git_files<cr>
nnoremap <leader>fF :Telescope find_files<cr>
nnoremap <leader>fb :Telescope file_browser<cr>
nnoremap <leader>fo :Telescope oldfiles<cr>
nnoremap <leader>fg :Telescope live_grep<cr>
nnoremap <leader>fr :Rename 
nnoremap <leader>fd :call ConfirmCmd("Really remove delete this file from disk?", ":Unlink")<cr>
nnoremap <leader>fD :call ConfirmCmd("Really permanently delete this file and buffer?", ":Delete")<cr>
nnoremap <leader>fs :w<cr>
nnoremap <leader>fS :SudoWrite<cr>

let g:which_key_map.f.e = { 'name': '+config',
            \ 'd': 'edit init.vim', 'r': 'reload init.vim',
            \ 'c': 'edit CoC config' }
nnoremap <leader>fed :e $MYVIMRC<cr>
nnoremap <leader>fer :source $MYVIMRC<cr>
nnoremap <leader>fec :e /home/forkk/.config/nvim/coc-settings.json<cr>

let g:which_key_map.b = { 'name' : '+buffers', 'b': 'find open buffer' }
nnoremap <leader>bb :Telescope buffers<cr>

let g:which_key_map.t = { 'name': '+tabs',
            \ 't': 'new tab', 'q': 'close tab', 'n': 'next tab', 'p': 'previous tab' }
nnoremap <leader>tt :tabnew<cr>
nnoremap <leader>tq :tabclose<cr>
nnoremap <leader>tn :tabnext<cr>
nnoremap <leader>tp :tabprev<cr>

let g:which_key_map.e = { 'name' : '+errors',
            \ 'l': 'show error list', 'c': 'hide error list',
            \ 'n': 'next error', 'p': 'prev error',
            \ 'r': 'restart CoC' }
nnoremap <leader>ee :CocRebuild<cr>
nnoremap <leader>ec :lclose<cr>
nnoremap <leader>el :CocDiagnostics<cr>
nnoremap <leader>en :lnext<cr>
nnoremap <leader>ep :lprev<cr>
nnoremap <leader>er :CocRestart<cr>

let g:which_key_map.s = { 'name' : '+snippets', 'e': 'edit snippets' }
nnoremap <leader>se :CocCommand snippets.editSnippets<cr>

let g:which_key_map.u = { 'name' : '+undo', 'u': 'toggle undo tree' }
nnoremap <leader>uu :MundoToggle<cr>

let g:which_key_map.g = { 'name' : '+git',
            \ 'g': 'fugitive UI in new tab', 'G': 'fugitive UI in split',
            \ 's': 'git status', 'd': 'git diff', 'l': 'git log',
            \ 'b': 'browse branches', 'u': 'toggle gitgutter' }
nnoremap <leader>gg :tab Git<cr>
nnoremap <leader>gG :Git<cr>
nnoremap <leader>gs :Git --paginate status<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gl :tab Git log<cr>
nnoremap <leader>gb :Twiggy<cr>
nnoremap <leader>gu :GitGutterToggle<cr>
nnoremap <leader>gU :GitGutterBufferToggle<cr>

" This folds all children of the current fold
nnoremap zs zcV:foldclose!<cr>zo

call which_key#register('<Space>', "g:which_key_map")

" }}}

" Status line {{{
function! IsFileChanged()
    let [a,m,r] = GitGutterGetHunkSummary()
    return a > 0 || m > 0 || r > 0
endfunction

function! GitChanges()
    let [a,m,r] = GitGutterGetHunkSummary()
    let changes = a > 0 || m > 0 || r > 0
    if changes
        return printf('+%d ~%d -%d', a, m, r)
    else
        return ''
    endif
endfunction

function! GitSummary()
    let gitsum=""
    let githead=FugitiveHead()
    let gitchanges = GitChanges()
    if githead != ""
        let gitsum=githead
    endif
    if gitchanges != ""
        let gitsum=printf('%s %s', gitsum, gitchanges)
    endif
    if gitsum == ""
        return ""
    else
        return printf('  %s ', gitsum)
    endif
endfunction

set statusline=
set statusline+=%#DiffAdd#%{(mode()[0]=='n')?'\ \ NORMAL\ ':''}
set statusline+=%#DiffAdd#%{(mode()[0]=='c')?'\ \ NORMAL\ ':''}
set statusline+=%#DiffChange#%{(mode()[0]=='i')?'\ \ INSERT\ ':''}
set statusline+=%#DiffDelete#%{(mode()[0]=='R')?'\ \ REPLACE\ ':''}
set statusline+=%#Visual#%{(mode()[0]=='v')?'\ \ VISUAL\ ':''}
set statusline+=%#Visual#%{(mode()=='V')?'\ \ V\ LINE\ ':''}
set statusline+=%#Visual#%{(mode()=='CTRL-V')?'\ \ VBLOCK\ ':''}
set statusline+=%#DiffText#%{IsFileChanged()?GitSummary():''}
set statusline+=%#Visual#%{IsFileChanged()?'':GitSummary()}
set statusline+=%*
set statusline+=\ %f\ %h%w%m%r
set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')}
set statusline+=%=
set statusline+=\ (%l,%c%V)\ 
" }}}

" }}}

"""" Plugin Settings {{{

"" Configure CoC {{{

" let g:coc_global_extensions = [
"         \ 'coc-snippets',
"         \ 'coc-ccls', 'coc-tsserver', 'coc-pyright', 'coc-solargraph',
"         \ 'coc-sh',
"         \ 'coc-html', 'coc-css', 'coc-json'
"       \ ]

" TextEdit might fail if hidden is not set.
set hidden

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Scroll popups with <C-j> and <C-k>
nnoremap <expr><C-j> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-j>"
nnoremap <expr><C-k> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-k>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> g] <Plug>(coc-diagnostic-prev)
nmap <silent> g[ <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rr <Plug>(coc-rename)
nmap <leader>ra <Plug>(coc-codeaction)

" Formatting selected code.
xmap <leader>gf <Plug>(coc-format-selected)
nmap <leader>gf <Plug>(coc-format-selected)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"


" Snippets
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

" Use <leader>x for convert visual selected code to snippet
xmap <leader>x  <Plug>(coc-convert-snippet)


"" }}}

" Neomake settings (disabled) {{{

"call neomake#configure#automake('w')

"let g:neomake_python_enabled_makers = ['python', 'mypy']
"let g:neomake_python_mypy_maker = {
"  \ 'exe': '/home/forkk/.local/bin/mypy'
"  \ }

""let g:neomake_open_list = 2
"let g:neomake_rust_cargo_exe = $HOME . '/.cargo/bin/cargo'
"let g:neomake_rust_cargo_args = neomake#makers#ft#rust#cargo().args + [ '--all-targets' ]

"let b:neomake_typescript_tsc_exe = $PWD .'/node_modules/.bin/tsc'
"let b:neomake_typescript_tsc_args = [ '--noEmit' ]
"let b:neomake_typescript_tsc_append_file = 1
"let b:neomake_typescript_tslint_exe = $PWD .'/node_modules/.bin/tslint'

" }}}

" Godot {{{
let g:godot_executable = '/home/forkk/bin/godot'
" }}}

" }}}

syntax on

