#!/bin/bash
#
# vim:ts=4:sw=4:sts=4:et:ft=sh:

hsetroot -solid "#000000"

xsetroot -cursor_name left_ptr

xset mouse 2 10

# sane clipboards?
xsel -o | xsel -b

# hide mouse cursor when it isn't moving
unclutter &

autocutsel -selection PRIMARY   -fork
autocutsel -selection CLIPBOARD -fork

# Set up an icon tray
trayer --edge  top --align     left  --margin 975 --expand     true \
       --width 100 --widthtype pixel --height 12  --heighttype pixel \
       --SetDockType true --SetPartialStrut true \
       --transparent true --tint 0x000000 &

# Start screen saver
xscreensaver &

dropbox start

keepassx -min -lock &

xcompmgr &

# XMonad tiling window manager
exec xmonad
