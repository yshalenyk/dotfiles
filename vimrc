set nocompatible
" Map leader
let mapleader = ","


" Plugins
call plug#begin('~/.vim/plugged')
	if !has('nvim')
		Plug 'tpope/vim-sensible'
	endif
	Plug 'Valloric/YouCompleteMe', { 'do': './install.py'  }
	Plug 'Valloric/python-indent' 
	Plug 'tpope/vim-surround'
	Plug 'jiangmiao/auto-pairs'
	Plug 'xolox/vim-misc'
	Plug 'xolox/vim-easytags'
	Plug 'majutsushi/tagbar'
	Plug 'vim-airline/vim-airline'
	Plug 'reedes/vim-colors-pencil'
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
set bg=light
colorscheme pencil
let g:airline_theme = 'pencil'
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
let g:netrw_browse_split = 2
let g:list_style=0

" folding

" Normal mode mappings
nnoremap <Space>  za " Toggle fold
nnoremap <leader>f :Vexplore<CR> " Toggle file manager
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

map <leader>l :set list!<CR> "show hidden


" Commands 
command! MakeTags !ctags -R . --python-kinds=-i
autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType python set makeprg=pylama\ --ignore\ E501\ % " lint code
