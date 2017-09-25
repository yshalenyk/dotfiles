#!/bin/bash

vimrcfile="$HOME/.vimrc" 
tmuxrcfile="$HOME/.tmux.conf"
neovimrc="$HOME/.config/nvim/init.vim"
ctagsrc="$HOME/.ctags"
gitconfig="$HOME/.gitconfig"
zshconfig="$HOME/.zshrc"
profile="$HOME/.zprofile"
aliases="$HOME/.alias"

for file in $vimrcfile $tmuxrcfile $neovimrc $ctagsrc $gitconfig $zshconfig $profile $aliases; do
	if [ -L $file ] || [ -e $file ]; then
		rm $file
	fi
done

if [[ ! -d  $HOME/.emacs.d ]]; then
	mkdir -p $HOME/.emacs.d
else 
	rm -rf $HOME/.emacs.d && mkdir -p $HOME/.emacs.d
fi

ln -s $(pwd)/vimrc $vimrcfile
ln -s $(pwd)/tmux.conf $tmuxrcfile
ln -s $(pwd)/vimrc $neovimrc
ln -s $(pwd)/ctags $ctagsrc
ln -s $(pwd)/gitconfig $gitconfig
ln -s $(pwd)/init.el $HOME/.emacs.d/init.el
ln -s $(pwd)/zshrc $HOME/.zshrc
ln -s $(pwd)/zprofile $HOME/.zprofile
ln -s "$(pwd)/alias.zsh" "$HOME/.alias"
