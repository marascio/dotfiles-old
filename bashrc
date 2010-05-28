
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

if [ '$TERM' = 'linux' ]; then
    echo -en '\e]P0222222' #black
    echo -en '\e]P8222222' #darkgrey
    echo -en '\e]P1803232' #darkred
    echo -en '\e]P9982b2b' #red
    echo -en '\e]P25b762f' #darkgreen
    echo -en '\e]PA89b83f' #green
    echo -en '\e]P3aa9943' #brown
    echo -en '\e]PBefef60' #yellow
    echo -en '\e]P4324c80' #darkblue
    echo -en '\e]PC2b4f98' #blue
    echo -en '\e]P5706c9a' #darkmagenta
    echo -en '\e]PD826ab1' #magenta
    echo -en '\e]P692b19e' #darkcyan
    echo -en '\e]PEa1cdcd' #cyan
    echo -en '\e]P7ffffff' #lightgrey
    echo -en '\e]PFdedede' #white
    clear #for background artifacting
fi

# Less Colors for Man Pages

export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

function env() {
    exec /usr/bin/env "$@" | grep -v ^LESS_TERMCAP_
}
