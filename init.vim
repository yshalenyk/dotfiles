set nocompatible

" Plugins:
call plug#begin('~/.vim/plugged')
  " additional languages
	Plug 'rust-lang/rust.vim'
	Plug 'tshirtman/vim-cython'

  " Tools
	Plug 'pearofducks/ansible-vim'
	Plug 'hashivim/vim-hashicorp-tools'
	Plug 'ekalinin/Dockerfile.vim'


  " Fuzzy search
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'

  " Autocomplete
  Plug 'ncm2/ncm2'
  Plug 'roxma/nvim-yarp'
  Plug 'ncm2/ncm2-bufword'
  Plug 'ncm2/ncm2-path'
  Plug 'ncm2/ncm2-tmux'
  Plug 'Shougo/neco-syntax'
  Plug 'ncm2/ncm2-syntax'
  Plug 'prabirshrestha/async.vim'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'ncm2/ncm2-vim-lsp'

  "Linting"
  Plug 'w0rp/ale'

  " Rest"
	Plug 'ervandew/supertab'

	Plug 'Konfekt/FastFold'
	Plug 'tmhedberg/SimpylFold'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'jacoborus/tender.vim'
	Plug 'NLKNguyen/papercolor-theme'
	" Plug 'kien/rainbow_parentheses.vim'

	Plug 'tpope/vim-surround'
	Plug 'jiangmiao/auto-pairs'
	Plug 'majutsushi/tagbar'
	Plug 'tomtom/tcomment_vim'
call plug#end()

" General
set splitbelow
set splitright

set hidden
set path+=**
set visualbell

" Apperiance
set number
set nowrap
set cursorline
set showmode
set showcmd
set t_Co=256   " This is may or may not needed.
set background=light
" colorscheme PaperColor
colorscheme tender

" let g:airline_theme = 'papercolor'
let g:airline_left_sep=''
let g:airline_right_sep=''
" let g:airline#extensions#tabline#enabled = 1

let g:python3_host_prog = '/home/linuxbrew/.linuxbrew/bin/python3'
let g:python_host_prog = '/home/linuxbrew/.linuxbrew/bin/python'

"" Enable folding
" Enable folding with the spacebar
nnoremap <space> za
let g:SimpylFold_docstring_preview=1
set foldmethod=indent
set foldlevel=99
nmap zuz <Plug>(FastFoldUpdate)
let g:fastfold_savehook = 1
let g:fastfold_fold_command_suffixes =  ['x','X','a','A','o','O','c','C']
let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']

" Search
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
" Plugins
"let g:easytags_async = 1
"let g:ctrlp_map = '<c-p>'
"let g:ctrlp_cmd = 'CtrlP'

set completeopt=noinsert,menuone,noselect
autocmd BufEnter * call ncm2#enable_for_buffer()

let g:tagbar_type_rust = {
    \ 'ctagstype' : 'rust',
    \ 'kinds' : [
        \'T:types,type definitions',
        \'f:functions,function definitions',
        \'g:enum,enumeration names',
        \'s:structure names',
        \'m:modules,module names',
        \'c:consts,static constants',
        \'t:traits,traits',
        \'i:impls,trait implementations',
    \]
    \}
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'
let g:lsc_server_commands = {'dart': 'dart_language_server'}
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"


" Normal mode mappings
" Just use default now
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>
nnoremap <c-q> :close<CR>


nnoremap <C-PAGEUP> :tabprevious <CR>
nnoremap <C-PAGEDOWN> :tabnext <CR>


nnoremap <C-Left> :tabprevious <CR>
nnoremap <C-Right> :tabnext <CR>

nnoremap <C-p>f :FZF<CR>
nnoremap <C-p>b :Buffers <CR>
nnoremap <C-p>s :Rg<CR>
nnoremap <C-p>g :GFiles<CR>
nnoremap <C-p>t :Tags<CR>
nnoremap <C-p>T :TagbarToggle<CR>
nnoremap <C-p>w :Windows<CR>
nnoremap <C-p>w :Marks<CR>

" Leader:
let mapleader = ","
" map <leader>l :set list!<CR> "show hidden


" Commands:
command! MakeTags !ctags -R --python-kinds=-i .

" no tabs
set tabstop=2
set shiftwidth=2
set expandtab
" Filetypes:
autocmd BufRead,BufNewFile *.zcml setlocal filetype=xml
autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType go set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType html,css EmmetInstall
autocmd FileType c set foldmethod=syntax
autocmd FileType haskell set foldmethod=indent


" let g:lsp_diagnostics_enabled = 1         " disable diagnostics support
" let g:lsp_signs_enabled = 1         " enable signs
" let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
"
" let g:lsp_signs_error = {'text': '✗'}
" let g:lsp_signs_warning = {'text': '‼'} " icons require GUI
" let g:lsp_signs_hint = {'test': '?'} " icons require GUI
"

if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

if executable('typescript-language-server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'typescript-language-server --stdio']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript'],
        \ })
  endif

if executable('docker-langserver')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'docker-langserver',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'docker-langserver --stdio']},
        \ 'whitelist': ['dockerfile'],
        \ })
  endif


if executable('flow')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'flow',
        \ 'cmd': {server_info->['flow', 'lsp']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), '.flowconfig'))},
        \ 'whitelist': ['javascript', 'javascript.jsx'],
        \ })
  endif


if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rls']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'Cargo.toml'))},
        \ 'whitelist': ['rust'],
        \ })
  endif
