tmux-editbuf(){
    local buffer=${1:-`tmux list-buffers -F '#{buffer_name}' | head -n 1`}
    local file=`tempfile`
    if tmux save-buffer -b "$buffer" $file ; then
    elif [[ -n $buffer ]] ; then
        return 1
    else
        sleep 0.5
    fi
    $EDITOR $file
    if [[ $(wc -c < $file) -eq 0 ]] ; then
        [[ -n $buffer ]] && tmux delete-buffer -b $buffer
    elif [[ -n $buffer ]]; then
        echo $buffer
        tmux load-buffer -b $buffer $file
    else
        tmux load-buffer $file
    fi
    rm $file
}
