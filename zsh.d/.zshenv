# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=before . $ZDOTDIR/.zshenv.local

# environment variables

typeset -U path
typeset -U manpath

path=($HOME/bin(N-/) $HOME/.cabal/bin(N-/) $path)
#manpath=(/usr/local/coq8.4pl1/share/man(N-/) $manpath)

# local configuration

[ -f $ZDOTDIR/.zshenv.local ] && POS=after . $ZDOTDIR/.zshenv.local
