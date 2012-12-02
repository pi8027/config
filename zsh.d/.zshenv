# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=before . $ZDOTDIR/.zshenv.local

# environment variables

typeset -U path

path=($HOME/bin(N-/) $HOME/.cabal/bin(N-/) $path)

# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=after . $ZDOTDIR/.zshenv.local
