#!/bin/bash

MKTDATA="$HOME/mktdata"
LOG="$HOME/log/mktdata-read-only.log"

files=$(find $MKTDATA -not -regex '.*/\..*' -type f -not -perm ug=r | sort)

#for i in $files; do
#    chmod 440 $i
#    echo "Fixed perms for $i" >> $LOG
#done

