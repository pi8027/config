# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=before . $ZDOTDIR/.zshenv.local

# environment variables

typeset -U path manpath cdpath fpath
typeset -T LS_COLORS ls_colors
typeset -T COQLIB coqlib " "

path=($HOME/bin(N-/) $HOME/.cabal/bin(N-/) $HOME/.local/bin(N-/) /opt/local/bin(N-/) $path)

# . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
eval $(opam config env)

# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=after . $ZDOTDIR/.zshenv.local
