#!/usr/bin/env bash
vol=$(amixer get Master | \
    awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print int($2/10)}}' | \
    head -n 1)

case $vol in
    0)    bar='[-----]' ;;
    1|2)  bar='[/----]' ;;
    3|4)  bar='[//---]' ;;
    5|6)  bar='[///--]' ;;
    7|8)  bar='[////-]' ;;
    9|10) bar='[/////]' ;;
    *)    bar='[--!--]' ;;
esac

echo Vol: $bar

exit 0
