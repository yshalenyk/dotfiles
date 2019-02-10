set nocompatible

" Plugins:
call plug#begin('~/.vim/plugged')
	Plug 'Valloric/YouCompleteMe', { 'do': './install.py --rust-completer --ts-completer' }
	Plug 'ervandew/supertab'
	Plug 'honza/vim-snippets'
	Plug 'SirVer/ultisnips'

	Plug 'rust-lang/rust.vim'
	Plug 'pearofducks/ansible-vim'

	Plug 'vim-syntastic/syntastic'

	Plug 'Konfekt/FastFold'
	Plug 'tmhedberg/SimpylFold'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'NLKNguyen/papercolor-theme'

	Plug 'tpope/vim-surround'
	Plug 'ctrlpvim/ctrlp.vim'
	Plug 'jiangmiao/auto-pairs'
	Plug 'xolox/vim-misc'
	Plug 'majutsushi/tagbar'
	Plug 'tomtom/tcomment_vim'
	Plug 'tshirtman/vim-cython'
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
colorscheme PaperColor
let g:airline_theme = 'papercolor'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline#extensions#tabline#enabled = 1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']


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
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

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

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"


" Normal mode mappings
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <c-q> :close<CR> 


nnoremap <C-PAGEUP> :tabprevious <CR> 
nnoremap <C-PAGEDOWN> :tabnext <CR> 


nnoremap <C-Left> :tabprevious <CR> 
nnoremap <C-Right> :tabnext <CR> 


" Leader:
let mapleader = ","
nnoremap <leader>m :Marks<CR> 
nnoremap <leader>w :Windows<CR> 
nnoremap <leader>f :Vexplore<CR> " Toggle file manager
map <leader>l :set list!<CR> "show hidden


" Commands:
command! MakeTags !ctags -R --python-kinds=-i . 

" Filetypes:
autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType html,css EmmetInstall


"python with virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF
