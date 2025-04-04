np(){
    local split_opts spawn_command

    while getopts dhvPp:l:t:b: OPT ; do
        case $OPT in
        "d" | "h" | "v" | "P" )
            split_opts="$split_opts -$OPT";;
        "p" | "l" | "t" )
            split_opts="$split_opts -$OPT $OPTARG";;
        * ) echo "Usage: $(basename $0) [-dhvP]" \
                 "[-p percentage|-l size] [-t target-pane] [command]" 1>&2
            return 1;;
        esac
    done
    shift `expr $OPTIND - 1`

    tmux split-window $(echo -n $split_opts) \
        -c $PWD "$(printf "%q " ${@:-$SHELL})"
}

_np(){
    local args
    args=(
        '-d[do not make the new window become the active one]'
        '-h[split horizontally]'
        '-v[split vertically]'
        '-l[define new pane'\''s size]: :_guard "[0-9]#" "numeric value"'
        '-p[define new pane'\''s size in percent]: :_guard "[0-9]#" "numeric value"'
        '-t[choose target pane]: :_guard "[0-9]#" "numeric value"'
        '*:: :_normal'
    )
    _arguments ${args} && return
}

compdef _np np
