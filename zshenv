# local configuration

[ -f ~/.zshenv.local ] && POS=before . ~/.zshenv.local

# environment variables

typeset -U path

path=($HOME/bin(N-/) $HOME/.cabal/bin(N-/) $path)

# local configuration

[ -f ~/.zshenv.local ] && POS=after . ~/.zshenv.local
