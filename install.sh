#!/bin/bash

vimrcfile="$HOME/.vimrc" 
tmuxrcfile="$HOME/.tmux.conf"
neovimrc="$HOME/.config/nvim/init.vim"
ctagsrc="$HOME/.ctags"
gitconfig="$HOME/.gitconfig"

for file in $vimrcfile $tmuxrcfile $neovimrc $ctagsrc; do
	if [ -L $file ] || [ -e $file ]; then
		rm $file
	fi
done

if [[ ! -d  $HOME/.emacs.d ]]; then
	mkdir $HOME/.emacs.d
fi

ln -s $(pwd)/vimrc $vimrcfile
ln -s $(pwd)/tmux.conf $tmuxrcfile
ln -s $(pwd)/vimrc $neovimrc
ln -s $(pwd)/ctags $ctagsrc
ln -s $(pwd)/gitconfig $gitconfig
ln -s $(pwd)/init.el $HOME/.emacs.d/init.el
