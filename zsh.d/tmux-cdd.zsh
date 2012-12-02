_set_tmuxpwd(){
    tmux setenv $(tmux display -p 'TMUXWD_#I') $PWD
    tmux setenv $(tmux display -p 'TMUXWD_#I_#P') $PWD
}

add-zsh-hook chpwd _set_tmuxpwd

cdd(){
    local name dir
    if [ -n "$1" ]; then
        if [ -n "$2" ]; then
            name="TMUXWD_$1_$2"
        else
            name="TMUXWD_$1"
        fi

        dir=`tmux show-environment $name | sed 's/^.*=//'`

        if [ -d "$dir" ]; then
            cd "$dir"
        else
            echo 'cdd : error' >&2
        fi
    else
        echo 'usage : cdd [window [pane]]' >&2
    fi
}
