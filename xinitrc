#!/bin/bash

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
       --expand true --width 10 --transparent true --tint 0x191970 --height 12 &

xscreensaver -no-splash &

exec xmonad
