#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

manifest="
bashrc:$HOME/.bashrc
emacs:$HOME/.emacs
gitconfig:$HOME/.gitconfig
xinitrc:$HOME/.xinitrc
xmobarrc:$HOME/.xmobarrc
xmonad.hs:$HOME/.xmonad/xmonad.hs
xresources:$HOME/.Xresources
site-lisp:/usr/share/emacs/site-lisp
gitcompletion.sh:$HOME/.git-completion.sh
"
for file in $manifest; do
src="${DIR}/${file%%:*}"
dest=${file##*:}
path=${dest%/*}
mkdir -p $path
ln -s $src $dest
done
