if [[ -z $ZDOTDIR ]]; then
    export ZDOTDIR=~/.zsh.d
fi

[[ -f $ZDOTDIR/.zshenv ]] && . $ZDOTDIR/.zshenv
