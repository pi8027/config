
source ~/.zshrc.local

# autoload

autoload -Uz colors ; colors
autoload -Uz compinit ; compinit
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
autoload -Uz is-at-least

# options

setopt correct
setopt list_packed
setopt nolistbeep

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent

# typeset

typeset -U cdpath fpath manpath

cdpath=(~)

# sed scripts

homerep_sedscript="s/`echo $HOME | sed -e "s/\\//\\\\\\\\\//g"`/~/g"

# tmux

if [ -z $TMUX ] && [ -z $WITHOUT_SCREEN ] && [ $TERM != "screen" ]; then
    tmux
    export WITHOUT_SCREEN=1
else
    function _update_title1(){
        echo -ne "\ek$(pwd | sed -e $homerep_sedscript)% $1\e\\"
    }

    function _update_title2(){
        echo -ne "\ek$(pwd | sed -e $homerep_sedscript)%\e\\"
    }

    function _tmux_alert(){
        echo -n "\a"
    }

    add-zsh-hook preexec _update_title1
    add-zsh-hook precmd _update_title2
    add-zsh-hook precmd _tmux_alert
fi

# prompt

if [ $TERM != "dumb" ]; then
    PROMPT="%U%F{red}[%n@%M]%f%u(%j)%# "
    PROMPT2="%F{red}%_%%%f "
    SPROMPT="%F{red}%r is correct? [n,y,a,e]:%f "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%F{cyan}$(echo ${HOST%%.*} | tr "[a-z]" "[A-Z]") ${PROMPT}"
else
    PROMPT="%U[%n@%M]%u(%j)%# "
    PROMPT2="%_%% "
    SPROMPT="%r is correct? [n,y,a,e]: "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="$(echo ${HOST%%.*} | tr "[a-z]" "[A-Z]") ${PROMPT}"
fi

function _update_rprompt() {
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        psvar=(`echo "$vcs_info_msg_0_" | sed -e $homerep_sedscript`
            "$vcs_info_msg_1_" "$vcs_info_msg_2_" "$vcs_info_msg_3_")
        [[ -n $psvar[4] ]] && psvar[4]=" "$psvar[4]
        RPROMPT="%F{green}[%1v:%2v] %F{red}%3v%f%4v"
    else
        RPROMPT="[%~]"
    fi
}

add-zsh-hook precmd _update_rprompt

# zle

#bindkey -v

# history

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history
setopt hist_ignore_all_dups

# comp

zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
zstyle ":completion:*:sudo:*" command-path \
    /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ":completion:*:cd:*" local-directories path-directories
LISTMAX=0

# alias

alias history-all="history -E 1 | less"
alias ls="ls -F --color"
alias emacs="emacs --no-window-system"
alias gosh="rlwrap gosh"
alias maxima="rlwrap maxima"
alias coqtop="rlwrap coqtop"
alias gs="rlwrap gs"
alias ghc="ghc --make"
alias hoogle="hoogle --color=true"
alias google="w3m http://google.com/"

# directory stack

alias pd="popd"
alias dirs="dirs -v"

function _print_dirstack(){
    \dirs -v
}

add-zsh-hook chpwd _print_dirstack

# cdd

function _set_tmuxpwd(){
    if [ -n "$TMUX" ]; then
        tmux setenv $(tmux display -p "TMUXPWD_#I") $PWD
        tmux setenv $(tmux display -p "TMUXPWD_#I_#P") $PWD
    fi
}

function cdd(){
    if [ -n "$1" ]; then
        if [ -n "$2" ]; then
            name="TMUXPWD_$1_$2"
        else
            name="TMUXPWD_$1"
        fi

        dir=`tmux getenv $name`

        if [ -d "$dir" ]; then
            cd "$dir"
        else
            echo "cdd : error"
        fi
    else
        echo "usage : cdd [window [pane]]"
    fi
}

add-zsh-hook chpwd _set_tmuxpwd

# make and change directory

function mpd(){
    mkdir -p $1
    cd $1
}

function daypd(){
    mpd `date "+%Y-%m-%d"`
}

# vcs_info

zstyle ":vcs_info:*" enable git svn hg bzr darcs
zstyle ":vcs_info:*" max-exports 4
zstyle ":vcs_info:*" check-for-changes true
zstyle ":vcs_info:*" stagedstr "+"
zstyle ":vcs_info:*" unstagedstr "-"
zstyle ":vcs_info:*" formats "%R" "%b" "%S" "%c%u"
zstyle ":vcs_info:*" actionformats "%R" "%b|%a" "%S" "%c%u"

function cdv(){
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        cd "$vcs_info_msg_0_/$1"
    else
        echo "vchome : Repository not found."
    fi
}

function _cdv(){
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        _files -/ -W "$vcs_info_msg_0_"
    fi
}

compdef _cdv cdv

