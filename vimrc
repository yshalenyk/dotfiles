set nocompatible


" Plugins:
call plug#begin('~/.vim/plugged')
	Plug 'Valloric/YouCompleteMe', { 'do': './install.py'  }
	if !has('nvim')
		Plug 'tpope/vim-sensible'
		Plug 'vim-syntastic/syntastic'
	endif
	Plug 'junegunn/goyo.vim'
	Plug 'mattn/emmet-vim'
	Plug 'flazz/vim-colorschemes'
	Plug 'Konfekt/FastFold'
	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'  }
	Plug 'Valloric/python-indent' 
	Plug 'tpope/vim-surround'
	Plug 'jiangmiao/auto-pairs'
	Plug 'xolox/vim-misc'
	Plug 'xolox/vim-easytags'
	Plug 'majutsushi/tagbar'
	Plug 'vim-airline/vim-airline'
	Plug 'reedes/vim-colors-pencil'
	Plug 'tomtom/tcomment_vim'
	Plug 'tshirtman/vim-cython'
	Plug 'morhetz/gruvbox'
	"Plug 'tmhedberg/SimpylFold'
call plug#end()

" General
set hidden
set path+=**
set visualbell
"et ttyfast

" Apperiance
set number
set nowrap
set cursorline
set showmode
set showcmd
set bg=dark
colorscheme busybee
let g:airline_theme = 'dark'
let g:airline_left_sep=''
let g:airline_right_sep=''
"
"set t_Co=256

" Folding
set foldenable
set foldlevelstart=10   " open most folds by default"
set foldnestmax=10
set foldmethod=indent

" Search 
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

" netrw file browser
let g:netrw_liststyle = 3
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:netrw_banner = 0 " disable banner on top
"let g:netrw_browse_split = 2
let g:list_style=0

" Plugins
let g:easytags_async = 1
let g:easytags_file = '~/.vim/tags'

let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_min_num_identifier_candidate_chars = 2

" Normal mode mappings
nnoremap <Space>  za " Toggle fold
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <c-q> :close<CR> 


nnoremap  <F2> :FZF<CR>
nnoremap <F3> :TagbarToggle<CR> 

nnoremap <C-PAGEUP> :tabprevious <CR> 
nnoremap <C-PAGEDOWN> :tabnext <CR> 


nnoremap <C-Left> :tabprevious <CR> 
nnoremap <C-Right> :tabnext <CR> 


" Leader:
let mapleader = ","
nnoremap <leader>f :Vexplore<CR> " Toggle file manager
map <leader>l :set list!<CR> "show hidden


" Commands:
command! MakeTags !ctags -R --python-kinds=-i . 

" Filetypes:
autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType python set makeprg=pylama\ --ignore\ E501\ % " lint code


"au BufAdd,BufNewFile * nested tab sball

let g:airline#extensions#tabline#enabled = 1


let g:syntastic_check_on_open = 1
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

" GUI:
if has('gui_running')
	"set guioptions-=m  "menu bar
	"set guioptions-=T  "toolbar
	"set guioptions-=r  "scrollbar"
	let g:fzf_launcher = 'konsole -e bash -ic %s'
	set guioptions=i
	set guifont=Source\ Code\ Pro\ 10
endif

" This is the default extra key bindings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }


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



let g:tex_fold_enabled=1
let g:vimsyn_folding='af'
let g:python_fold = 1

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

"let g:user_emmet_leader_key='<C-l>'
