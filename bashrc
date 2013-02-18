# bashrc - configure non-login bash shells
# 
# vim:ts=4:sw=4:sts=4:et:ft=sh:

export LANG="en_US.UTF-8"

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion
source ~/.git-prompt.sh

# Check for an interactive session
[ -z "$PS1" ] && return

PS1="[\u@\h \$(__git_ps1 '\[\033[1;32m\](%s)\[\033[0m\] ')\w]\$ "

if [ ! -z "$SCHROOT_USER" ]; then 
    WHICH_CHROOT=$(echo $SCHROOT_SESSION_ID | awk -F - '{print $1}')
    PS1="[\u@\h\[\033[1;35m\]<$WHICH_CHROOT>\[\033[0m\] \$(__git_ps1 '\[\033[1;32m\](%s)\[\033[0m\] ')\w]\$ "
fi

# Make sure we sort directory listings sanely
export LC_ALL=
export LC_COLLATE="C"

# Colors
if [ -r $HOME/.dir_colors ]; then
    eval $(dircolors -b $HOME/.dir_colors)
fi

# Setup Tariten environment
[ -f "$HOME/.tt/ttsetup/ttenv.sh" ] && source $HOME/.tt/ttsetup/ttenv.sh

# Path
[ -d "$HOME/code/dotfiles/bin" ] && PATH="$HOME/code/dotfiles/bin:$PATH"
[ -d "$HOME/.gem/ruby/1.9.1/bin" ] && PATH="$HOME/.gem/ruby/1.9.1/bin:$PATH"
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"
[ -d "$HOME/tt/bin" ] && PATH="$HOME/tt/bin:$PATH"
export PATH

[ -d "$HOME/opt/lib" ] && LD_LIBRARY_PATH="$HOME/opt/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

export R_LIBS_USER="$HOME/opt/R/library"

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
alias lspeed-up='sudo vpnc --local-port=0 lightspeed-gr'
alias lspeed-down='sudo vpnc-disconnect'
alias mktclock='xclock -digital -update 1 -face "terminus-30:bold" -geometry 510x50+1085+4'
alias monitorlsgw='sudo ngrep -d lo -tq -W byline -v "(^H|^R)" port 31091'

if [ "$(hostname)" = "cobra" ]; then
    alias startx='LD_PRELOAD=/home/lrm/opt/fakexinerama/libXinerama.so startx'
fi

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
hfind() {
    if [ -z "$1" ]; then
        echo 'Usage: hfind STRING'
        return 1
    fi

    history | grep '$@'
}

switchtaritenaws() {
    local acct=$1
    if [ -z "$acct" ]; then
        echo "usage: switchtaritenaws ACCOUNT"
        return 1
    fi

    local ak=$(awk "/$acct :.*$/,/};/" $HOME/.tt/prod.cfg | grep access_key | awk -F"=" '{print $2}')
    local sk=$(awk "/$acct :.*$/,/};/" $HOME/.tt/prod.cfg | grep secret_key | awk -F"=" '{print $2}')
    export EC2_CERT=$HOME/.tt/prod/$acct.cert.pem
    export EC2_PRIVATE_KEY=$HOME/.tt/prod/$acct.cert-pk.pem
    ak=$(echo "$ak" | tr -d '"; ')
    sk=$(echo "$sk" | tr -d '"; ')
    echo "AWSAccessKeyId=$ak" > ~/.awscredentials
    echo "AWSSecretKey=$sk" >> ~/.awscredentials
    export AWS_CREDENTIAL_FILE=$HOME/.awscredentials
    export AWS_ACCESS_KEY_ID=$ak
    export AWS_SECRET_ACCESS_KEY=$sk
}

eval `keychain -q --eval --agents ssh id_rsa`
