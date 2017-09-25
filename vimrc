set nocompatible


" Plugins:
call plug#begin('~/.vim/plugged')
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
	Plug 'zchee/deoplete-jedi'

	Plug 'rust-lang/rust.vim'
	Plug 'racer-rust/vim-racer'

	Plug 'w0rp/ale'
	Plug 'sebastianmarkow/deoplete-rust'

	Plug 'pearofducks/ansible-vim'
	Plug 'jacoborus/tender.vim'
	Plug 'Konfekt/FastFold'

	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'  }
	Plug 'junegunn/fzf.vim'
	Plug 'tpope/vim-surround'

	Plug 'jiangmiao/auto-pairs'
	Plug 'xolox/vim-misc'

	Plug 'majutsushi/tagbar'
	Plug 'vim-airline/vim-airline'
	Plug 'tomtom/tcomment_vim'

	Plug 'tshirtman/vim-cython'

	Plug 'morhetz/gruvbox'
call plug#end()

" General
set hidden
set path+=**
set visualbell
"et ttyfast

" Apperiance
colorscheme tender
set number
set nowrap
set cursorline
set showmode
set showcmd
set bg=dark
let g:airline_theme = 'dark'
let g:airline_left_sep=''
let g:airline_right_sep=''

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
"
"set t_Co=256
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
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

let g:necoghc_use_stack = 1

" Normal mode mappings
nnoremap <Space>  za " Toggle fold
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <c-q> :close<CR> 


nnoremap <F2> :FZF<CR>
nnoremap <F3> :F<CR> 
nnoremap <F4> :Buffers<CR> 
nnoremap <F5> :BTags<CR> 
nnoremap <F6> :Tags<CR> 


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
autocmd FileType haskell set tabstop=4|set shiftwidth=4|set expandtab " No spaces
autocmd FileType cabal set tabstop=4|set shiftwidth=4|set expandtab " No spaces


let g:rg_command = '
  \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
  \ -g "*.{js,json,php,md,styl,jade,html,config,py,cpp,c,go,hs,rb,conf}"
  \ -g "!{.git,node_modules,vendor}/*" '

command! -bang -nargs=* F call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)


let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#ale#enabled = 1

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
let g:racer_cmd = "/home/yshalenyk/.cargo/bin/racer"

"let g:user_emmet_leader_key='<C-l>'
