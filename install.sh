#!/bin/bash

vimrcfile="$HOME/.vimrc" 
tmuxrcfile="$HOME/.tmux.conf"
neovimrc="$HOME/.config/nvim/init.vim"

for file in $vimrcfile $tmuxrcfile $neovimrc; do
	if [ -L $file ] || [ -e $file ]; then
		rm $file
	fi
done

ln -s $(pwd)/vimrc $vimrcfile
ln -s $(pwd)/tmux.conf $tmuxrcfile
ln -s $(pwd)/vimrc $neovimrc
