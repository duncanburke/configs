#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

manifest="
bashrc:$HOME/.bashrc
zshrc:$HOME/.zshrc
emacs:$HOME/.emacs
gitconfig:$HOME/.gitconfig
gitignore:$HOME/.gitignore_global
xinitrc:$HOME/.xinitrc
xmobarrc:$HOME/.xmobarrc
xmonad.hs:$HOME/.xmonad/xmonad.hs
xresources:$HOME/.Xresources
site-lisp:/usr/share/emacs/site-lisp
gitcompletion.sh:$HOME/.git-completion.sh
ncmpcpp:$HOME/.ncmpcpp/config
"
for file in $manifest; do
src="${DIR}/${file%%:*}"
dest=${file##*:}
path=${dest%/*}
mkdir -p $path
ln -s $src $dest
done
