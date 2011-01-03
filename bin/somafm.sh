#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    echo "error: please specify a stream"
    cat << EOF
    groovesalad    dronezone       indiepop        lush
    secretagent    spacestation    beatblender     christmas
    bootliquor     suburbsofgoa    sonicuniverse   tags
    poptron        u80s            cliqhop         illstreet
    digitalis      missioncontrol  xmasinfrisko    doomed
    covers         brfm
EOF
    exit 1
fi

stream=$1
streamurl="http://somafm.com/startstream=$stream.pls"
playedurl="http://somafm.com/play/$stream"
nohup mplayer -really-quiet -vo none -ao alsa $streamurl &>/dev/null &
pid=$!
watch -n5 "links -dump $playedurl | sed -n '/Music Director.*/,/SomaFM/p'"
kill $pid
