# autoload

autoload -Uz colors ; colors
autoload -Uz compinit ; compinit
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
autoload -Uz is-at-least
autoload -Uz zargs

# options

setopt correct
setopt list_packed
setopt nolistbeep

setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_silent
setopt extended_glob

# typeset

typeset -U cdpath fpath manpath

cdpath=(~)

# tmux

if [ -n "$TMUX" ] ; then
    _tmux-set-title(){
        echo -ne "\033k$1\033\\"
        echo -ne "\033]2;$1\033\\"
    }

    _update_title1(){
        _tmux-set-title "$(print -D $(pwd))% $1"
    }

    _update_title2(){
        _tmux-set-title "$(print -D $(pwd))%"
    }

    _tmux_alert(){
        echo -n "\a"
    }

    add-zsh-hook preexec _update_title1
    add-zsh-hook precmd _update_title2
    add-zsh-hook precmd _tmux_alert

    source ~/.zsh/nw.zsh
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

_update_rprompt(){
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        psvar=(`print -D "$vcs_info_msg_0_"`
            "$vcs_info_msg_1_" "$vcs_info_msg_2_" "$vcs_info_msg_3_")
        [[ -n $psvar[4] ]] && psvar[4]=" "$psvar[4]
        RPROMPT="%F{green}[%1v:%2v] %F{red}%3v%f%4v"
    else
        RPROMPT="[%~]"
    fi
}

add-zsh-hook precmd _update_rprompt

# zle

bindkey -e

# history

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history
setopt hist_ignore_all_dups

# completion

zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
zstyle ":completion:*:sudo:*" command-path \
    /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ":completion:*:cd:*" tag-order local-directories path-directories
LISTMAX=0

# alias

alias history-all="history -E 1 | less"
alias less="less -r -M"
alias emacs="emacs --no-window-system"
alias gosh="rlwrap gosh"
alias racket="rlwrap racket"
alias ocaml="rlwrap ocaml"
alias coqtop="rlwrap coqtop"
alias gs="rlwrap gs"
alias hoogle="hoogle --color=true"
alias google="w3m http://google.com/"

case "${OSTYPE}" in
linux*)
  alias ls="ls -F --color"
  ;;
*bsd*|darwin*)
  alias ls="ls -F -G"
  ;;
esac
alias ll="ls -l"
alias la="ls -la"

# directory stack

alias pd="popd"
alias dirs="dirs -v"

_print_dirstack(){
    dirs
}

add-zsh-hook chpwd _print_dirstack

# cdd

_set_tmuxpwd(){
    if [ -n "$TMUX" ]; then
        tmux setenv $(tmux display -p "TMUXWD_#I") $PWD
        tmux setenv $(tmux display -p "TMUXWD_#I_#P") $PWD
    fi
}

cdd(){
    local name dir
    if [ -n "$1" ]; then
        if [ -n "$2" ]; then
            name="TMUXWD_$1_$2"
        else
            name="TMUXWD_$1"
        fi

        dir=`tmux show-environment $name | sed "s/^.*=//"`

        if [ -d "$dir" ]; then
            cd "$dir"
        else
            echo "cdd : error" >&2
        fi
    else
        echo "usage : cdd [window [pane]]" >&2
    fi
}

add-zsh-hook chpwd _set_tmuxpwd

# make and change directory

mpd(){
    mkdir -p $1
    cd $1
}

daypd(){
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

cdv(){
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        cd "$vcs_info_msg_0_/$1"
    else
        echo "vchome : Repository not found." >&2
    fi
}

_cdv(){
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        _files -/ -W "$vcs_info_msg_0_"
    fi
}

compdef _cdv cdv

# clipboard editor

clipedit(){
    file=`tempfile`
    clipboardout > $file
    $EDITOR $file
    cat $file | clipboardin
    rm $file
}

# tmux buffer

tmux-loadcb(){
    clipboardout | tmux load-buffer $@ -
}

tmux-storecb(){
    tmux save-buffer $@ - | clipboardin
}

tmux-editbuf(){
    buffer=$(echo "$1" | sed 's/^$/0/')
    echo "$buffer" | grep -Eqv "^[0-9]{1,}$" && return 1
    file=`tempfile`
    tmux save-buffer -b $buffer $file
    $EDITOR $file
    tmux load-buffer -b $buffer $file
    rm $file
}

# local configuration

[ -f ~/.zshrc.local ] && . ~/.zshrc.local
