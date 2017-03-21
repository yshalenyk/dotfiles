set nocompatible
" Map leader
let mapleader = ","


" Plugins
call plug#begin('~/.vim/plugged')
	Plug 'tpope/vim-sensible'
	Plug 'Valloric/YouCompleteMe', { 'do': './install.py'  }
	Plug 'Valloric/python-indent' 
	Plug 'tpope/vim-surround'
	Plug 'jiangmiao/auto-pairs'
	Plug 'xolox/vim-misc'
	Plug 'xolox/vim-easytags'
	Plug 'majutsushi/tagbar'
	Plug 'vim-airline/vim-airline'
	Plug 'reedes/vim-colors-pencil'
call plug#end()

" General
set hidden
set path+=**
set visualbell
"et ttyfast

" Apperiance
set number
set showmode
set showcmd
set bg=light
colorscheme pencil
let g:airline_theme = 'pencil'
let g:airline_left_sep=''
let g:airline_right_sep=''
"
"set t_Co=256

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



" Normal mode mappings
nnoremap <leader>f :Vexplore<CR>
nnoremap j gj
nnoremap k gk
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

map <leader>l :set list!<CR>
" Commands 
command! MakeTags !ctags -R .
autocmd FileType python set tabstop=4|set shiftwidth=4|set expandtab
autocmd FileType python set makeprg=pylama\ --ignore\ E501\ %
