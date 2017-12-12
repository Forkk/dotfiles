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
Plug 'vim-syntastic/syntastic'

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

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

nnoremap <leader>ff :Denite file_rec<cr>
nnoremap <leader>fr :Rename 
nnoremap <leader>fd :call ConfirmCmd("Really remove delete this file from disk?", ":Unlink")<cr>
nnoremap <leader>fD :call ConfirmCmd("Really permanently delete this file and buffer?", ":Delete")<cr>
nnoremap <leader>fs :w<cr>
nnoremap <leader>fS :SudoWrite<cr>
nnoremap <leader>fed :e $MYVIMRC<cr>
nnoremap <leader>fer :source $MYVIMRC<cr>

nnoremap <leader>en :lnext<cr>
nnoremap <leader>ep :lprev<cr>

nnoremap <leader>bb :Denite buffer<cr>

nnoremap <leader>c gc

" Auto-reload init.vim
"augroup AutoCommands
"    autocmd BufWritePost $MYVIMRC source $MYVIMRC
"augroup END

" }}}

" }}}

"""" Plugin Settings {{{

"" Misc Plugins {{{

" Set up submodes
"call submode#enter_with('windows', 'n', '', '<leader>wm')
"call submode#leave_with('windows', 'n', '', '<Esc>')
"call submode#map('windows', 'n', '', 'l', '<C-w><lt>')
"call submode#map('windows', 'n', '', 'h', '<C-w><gt>')

" Set up Syntastic
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

" }}}

"" Configure deoplete {{{
let g:deoplete#enable_at_startup = 1

" Fix deoplete colors
highlight Pmenu ctermfg=7 ctermbg=8 guifg=#000000 guibg=#606060
"highlight PmenuSel ctermbg=1 guifg=#dddd00 guibg=#1f82cd
"highlight PmenuSbar ctermbg=0 guibg=#d6d6d6

" Set up C completion
"let g:deoplete#sources#clang#libclang_path = '/usr/lib64/llvm/5/lib64/libclang.so'

" Set up rust completion
let g:deoplete#sources#rust#racer_binary='/usr/bin/racer'
let g:deoplete#sources#rust#rust_source_path='/home/forkk/rust-src'

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

" }}}

syntax on

