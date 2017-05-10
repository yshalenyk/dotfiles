#!/bin/bash

vimrcfile="$HOME/.vimrc" 
tmuxrcfile="$HOME/.tmux.conf"
neovimrc="$HOME/.config/nvim/init.vim"
ctagsrc="$HOME/.ctags"

for file in $vimrcfile $tmuxrcfile $neovimrc $ctagsrc; do
	if [ -L $file ] || [ -e $file ]; then
		rm $file
	fi
done

ln -s $(pwd)/vimrc $vimrcfile
ln -s $(pwd)/tmux.conf $tmuxrcfile
ln -s $(pwd)/vimrc $neovimrc
ln -s $(pwd)/ctags $ctagsrc
