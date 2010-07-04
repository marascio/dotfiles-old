#!/bin/bash
#
# vim:ts=4:sw=4:sts=4:et:ft=sh:

xsetroot -cursor_name left_ptr

xset mouse 2 10

# hide mouse cursor when it isn't moving
unclutter &

# Set up an icon tray
trayer --edge   top  --align right --SetDockType true --SetPartialStrut true \
       --expand true --width 10    --transparent true --tint 0x191970        \
       --height 12 &

# Start screen saver
xscreensaver &

# XMonad tiling window manager
exec xmonad
