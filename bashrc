# bashrc - configure non-login bash shells
# 
# vim:ts=4:sw=4:sts=4:et:ft=sh:

# Check for an interactive session
[ -z "$PS1" ] && return

PS1="[\u@\h \$(__git_ps1 '\[\033[1;32m\](%s)\[\033[0m\] ')\w]\$ "

if [ ! -z "$SCHROOT_USER" ]; then 
    WHICH_CHROOT=$(echo $SCHROOT_SESSION_ID | awk -F - '{print $1}')
    PS1="[\u@\h\[\033[1;35m\]<$WHICH_CHROOT>\[\033[0m\] \$(__git_ps1 '\[\033[1;32m\](%s)\[\033[0m\] ')\w]\$ "
fi

# Setup Tariten environment
#[ -f "$HOME/.ttsetup/ttenv.sh" ] && source $HOME/.ttsetup/ttenv.sh

# Make sure we sort directory listings sanely
export LC_ALL=
export LC_COLLATE="C"

# Colors
if [ -r $HOME/.dir_colors ]; then
    eval $(dircolors -b $HOME/.dir_colors)
fi

# Path
[ -d "$HOME/opt/libsvm" ] && PATH="$HOME/opt/libsvm:$HOME/opt/libsvm/tools:$PATH"
[ -d "$HOME/opt/mingw" ] && PATH="$HOME/opt/mingw/usr/bin:$PATH"
[ -d "$HOME/code/dotfiles/bin" ] && PATH="$HOME/code/dotfiles/bin:$PATH"
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"
export PATH

[ -d "$HOME/opt/lib" ] && LD_LIBRARY_PATH="$HOME/opt/lib:$LD_LIBRARY_PATH"
[ -d "$HOME/opt/tt/linux/boost/lib" ] && LD_LIBRARY_PATH="$HOME/opt/tt/linux/boost/lib:$LD_LIRARY_PATH"
[ -d "$HOME/opt/tt/linux/poco/lib" ] && LD_LIBRARY_PATH="$HOME/opt/tt/linux/poco/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

# Editor and Pager
export EDITOR='vim' 
export VISUAL=$EDITOR
export PAGER='less'

export BROWSER='elinks'
[ "$DISPLAY" ] && export BROWSER='firefox'

export GREP_COLOR='1;32'

export GTEST_COLOR='yes'

shopt -s cdspell checkwinsize dotglob histappend
set   -o vi

alias df='df -h'
alias ls='ls -h --color=auto'
alias grep='grep --color=auto'
alias more='less'
alias pacman='sudo pacman'
alias vi='vim'
alias wikidiary='vim -S $HOME/.vim/sessions/wikidiary'
alias make='make -e'

#if [ -z "$SCHROOT_USER" ]; then 
#    export WINELOADER="$HOME/bin/wine-chroot"
#fi;

export GIT_PS1_SHOWDIRTYSTATE=1

if [ "$TERM" = "linux" ]; then
    echo -en '\e]P0000000' #black
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

# ---------------------------------------------------------------------------
# History
# ---------------------------------------------------------------------------

export HISTCONTROL='ignoredups'
export HISTSIZE=5000
export HISTIGNORE="&:pwd:exit:clear"
#export PROMPT_COMMAND='history -a && $PROMPT_COMMAND'
unset  HISTFILESIZE
alias  hrun='fc -s'                              # Run cmd from history

# Search history for a command matching substring
function hfind() {
    if [ -z "$1" ]; then
        echo 'Usage: hfind STRING'
        return
    fi

    history | grep '$@'
}

