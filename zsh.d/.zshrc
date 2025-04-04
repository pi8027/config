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

# variable types

export LANG PATH MANPATH LS_COLORS

# local configuration

[ -f $ZDOTDIR/.zshrc.local ] && POS=before . $ZDOTDIR/.zshrc.local

# variables

cdpath=(~ $cdpath)

LANG=en_US.UTF-8
ls_colors=(no=00 fi=00 di=04\;34 ln=01\;36 pi=40\;33
    so=40\;33 bd=40\;33 cd=40\;33 ex=01\;31 or=04\;36)

if which less > /dev/null ; then
    export PAGER="less"
    export LESS="--raw-control-chars"
fi

if which vim > /dev/null ; then
    export EDITOR=vim
fi

# prompt

if [[ $TERM != dumb ]]; then
    PROMPT='%U%F{red}[%n@%M]%f%u(%j)%# '
    PROMPT2='%F{red}%_%%%f '
    SPROMPT='%F{red}%r is correct? [n,y,a,e]:%f '
    [[ -n ${REMOTEHOST}${SSH_CONNECTION} ]] &&
        PROMPT="%F{cyan}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
else
    PROMPT='%U[%n@%M]%u(%j)%# '
    PROMPT2='%_%% '
    SPROMPT='%r is correct? [n,y,a,e]: '
    [[ -n ${REMOTEHOST}${SSH_CONNECTION} ]] &&
        PROMPT="$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
fi

_update_rprompt(){
    LANG=en_US.UTF-8 vcs_info
    if [[ -n $vcs_info_msg_0_ ]]; then
        psvar=("$(print -D "$vcs_info_msg_0_")"
            "$vcs_info_msg_1_" "$vcs_info_msg_2_" "$vcs_info_msg_3_")
        [[ -n $psvar[4] ]] && psvar[4]=" $psvar[4]"
        RPROMPT='%F{green}[%1v:%2v] %F{red}%3v%f%4v'
    else
        RPROMPT='[%~]'
    fi
}

add-zsh-hook precmd _update_rprompt

# zle

bindkey -e

# history

HISTFILE=$ZDOTDIR/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history
setopt hist_ignore_all_dups

# completion

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:sudo:*' command-path \
    /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
LISTMAX=0

# alias

alias history-all='history -E 1 | less'
alias less='less -r -M'
alias gosh='rlwrap gosh'
alias ocaml='rlwrap ocaml'
alias coqtop='rlwrap coqtop'
alias gs='rlwrap gs'
alias hoogle='hoogle --color=true'
alias google='w3m http://google.com/'

alias ls='ls -F'
case "$OSTYPE" in
linux*)
  alias ls='ls --color=auto' ;;
darwin*)
  alias ls='ls -G' ;;
esac
alias ll='ls -l'
alias la='ls -la'

# directory stack

alias pd='popd'
alias dirs='dirs -v'

_print_dirstack(){
    dirs
}

add-zsh-hook chpwd _print_dirstack

# make and change directory

mpd(){
    mkdir -p $1
    cd $1
}

daypd(){
    mpd `date '+%Y-%m-%d'`
}

# tmux

if [[ -n $TMUX ]] ; then

    # title

    _tmux-set-title(){
        echo -ne "\033k$1\033\\"
        echo -ne "\033]2;$1\033\\"
    }

    _update_title1(){
        local arr
        arr=(${=1})
        if [[ ${arr[1]} = 'fg' ]]; then
            _tmux-set-title "$(print -D $(pwd))%(fg) ${jobtexts[${arr[2]:-%+}]}"
        else
            _tmux-set-title "$(print -D $(pwd))% $1"
        fi
    }

    _update_title2(){
        _tmux-set-title "$(print -D $(pwd))%"
    }

    _tmux_alert(){
        echo -n '\a'
    }

    add-zsh-hook preexec _update_title1
    add-zsh-hook precmd _update_title2
    add-zsh-hook precmd _tmux_alert

    # buffer

    tmux-loadcb(){
        clipboardout | tmux load-buffer $@ -
    }

    tmux-storecb(){
        tmux save-buffer $@ - | clipboardin
    }

    # other

    . $ZDOTDIR/tmux-cdd.zsh
    . $ZDOTDIR/tmux-np.zsh
    . $ZDOTDIR/tmux-editbuf.zsh
fi

# vcs_info

zstyle ':vcs_info:*' enable git svn hg bzr darcs
zstyle ':vcs_info:*' max-exports 4
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' unstagedstr '-'
zstyle ':vcs_info:*' formats '%R' '%b' '%S' '%c%u'
zstyle ':vcs_info:*' actionformats '%R' '%b|%a' '%S' '%c%u'

cdv(){
    LANG=en_US.UTF-8 vcs_info
    if [[ -n $vcs_info_msg_0_ ]]; then
        cd "$vcs_info_msg_0_/$1"
    else
        echo 'vchome : Repository not found.' >&2
    fi
}

_cdv(){
    LANG=en_US.UTF-8 vcs_info
    [[ -n $vcs_info_msg_0_ ]] && _files -/ -W "$vcs_info_msg_0_"
}

compdef _cdv cdv

# clipboard editor

clipedit(){
    local file=`tempfile`
    clipboardout > $file
    $EDITOR $file
    cat $file | clipboardin
    rm $file
}

# local configuration

[ -f $ZDOTDIR/.zshrc.local ] && POS=after . $ZDOTDIR/.zshrc.local
