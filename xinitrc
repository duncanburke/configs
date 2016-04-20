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

# Disable screensaver, turn off screen after 1 hour
# view settings with `xset q`, see xset(1)
xset s off
xset dpms 0 0 3600

exec xmonad
