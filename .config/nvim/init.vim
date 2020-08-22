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
Plug 'Shougo/denite.nvim'
Plug 'kana/vim-submode'
Plug 'raimondi/delimitmate'
" Plug 'vim-airline/vim-airline'

" Code Completion
Plug 'Shougo/deoplete.nvim'
Plug 'tweekmonster/deoplete-clang2'
" Plug 'zchee/deoplete-clang'
Plug 'sebastianmarkow/deoplete-rust'

" Syntax Checking
Plug 'neomake/neomake'

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

" Languages
Plug 'rust-lang/rust.vim'

call plug#end()

" }}}

"""" Basic settings {{{

set nocompatible
set tabstop=4
set shiftwidth=4
set softtabstop=0 expandtab

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

" Leader key shortcuts are configured to roughly resemble spacemacs.

nnoremap <leader>nn :call NumberToggle()<cr>
nnoremap <leader>nr :call RelNumberToggle()<cr>

nnoremap <leader>w <C-w>

nnoremap <leader>sc :nohl<cr>
nnoremap <leader>so :syntax on<cr>

nnoremap <leader>ff :Denite file/rec<cr>
nnoremap <leader>fr :Rename 
nnoremap <leader>fd :call ConfirmCmd("Really remove delete this file from disk?", ":Unlink")<cr>
nnoremap <leader>fD :call ConfirmCmd("Really permanently delete this file and buffer?", ":Delete")<cr>
nnoremap <leader>fs :w<cr>
nnoremap <leader>fS :SudoWrite<cr>
nnoremap <leader>fed :e $MYVIMRC<cr>
nnoremap <leader>fer :source $MYVIMRC<cr>

nnoremap <leader>ee :Neomake<cr>
nnoremap <leader>ec :lclose<cr>
nnoremap <leader>el :lopen<cr>
nnoremap <leader>en :lnext<cr>
nnoremap <leader>ep :lprev<cr>

" This folds all children of the current fold
nnoremap zs zcV:foldclose!<cr>zo

nnoremap <leader>bb :Denite buffer<cr>

nnoremap <leader>c gc

" Auto-reload init.vim
"augroup AutoCommands
"    autocmd BufWritePost $MYVIMRC source $MYVIMRC
"augroup END

" }}}

" }}}

"""" Plugin Settings {{{

"" Configure deoplete {{{
let g:deoplete#enable_at_startup = 1

" Fix deoplete colors
highlight Pmenu ctermfg=7 ctermbg=8 guifg=#000000 guibg=#606060

" Set up C completion
"let g:deoplete#sources#clang#libclang_path = '/usr/lib64/llvm/5/lib64/libclang.so'

" Set up rust completion
"let g:deoplete#sources#rust#racer_binary='/home/forkk/.cargo/bin/racer'
"let g:deoplete#sources#rust#rust_source_path='/home/forkk/rust-src/src'

" Set up Python
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

let g:lsp_log_verbose = 1
let g:lsp_log_file = expand('~/vim-lsp.log')

" }}}

" Denite settings {{{

call denite#custom#filter('matcher/ignore_globs', 'ignore_globs',
	      \ [ '.git/', '.ropeproject/', '__pycache__/',
	      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/'])

autocmd FileType denite call s:denite_my_settings()
function! s:denite_my_settings() abort
  nnoremap <silent><buffer><expr> <CR>
  \ denite#do_map('do_action')
  nnoremap <silent><buffer><expr> p
  \ denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> q
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> <Space>
  \ denite#do_map('toggle_select').'j'
endfunction

" }}}

" Neomake settings {{{

call neomake#configure#automake('w')

let g:neomake_python_enabled_makers = ['python', 'mypy']
let g:neomake_python_mypy_maker = {
  \ 'exe': '/home/forkk/.local/bin/mypy'
  \ }

"let g:neomake_open_list = 2
let g:neomake_rust_cargo_exe = $HOME . '/.cargo/bin/cargo'
let g:neomake_rust_cargo_args = neomake#makers#ft#rust#cargo().args + [ '--all-targets' ]

" }}}

" }}}

syntax on

