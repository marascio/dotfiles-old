#!/usr/bin/env bash

test $(type -P links) || { echo "error: 'links' must be installed"; exit 1; }

if [ $# -lt 1 ]; then
    echo "error: please specify a stream"
    cat << EOF
    groovesalad    dronezone       indiepop        lush
    secretagent    spacestation    beatblender     christmas
    bootliquor     suburbsofgoa    sonicuniverse   tags
    poptron        u80s            cliqhop         illstreet
    digitalis      missioncontrol  xmasinfrisko    doomed
    covers         brfm            deepspaceone    sf1033

    Visit: http://somafm.com/ for a list of streams and descriptions
EOF
    exit 1
fi

stream=$1
streamurl="http://somafm.com/startstream=$stream.pls"
playedurl="http://somafm.com/$stream/played"
mplayer -allow-dangerous-playlist-parsing \
        -really-quiet -vo none -ao alsa $streamurl &>/dev/null &
pid=$!
trap 'kill -9 $pid;' TERM QUIT INT
watch -n5 -t "links -dump $playedurl | sed -n '/.*:[.\n]*/,/SomaFM time is:/p'"
