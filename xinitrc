#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

urxvtd -q -o -f

xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
xcompmgr&
feh --bg-scale .wallpaper.png&
wicd-client&

exec xmonad
