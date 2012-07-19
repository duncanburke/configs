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
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 1 --height 12 --transparent true --tint 0x000000 &
wicd-client&

exec xmonad
