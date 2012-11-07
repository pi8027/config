typeset -U path

export LANG=ja_JP.UTF-8
export LS_COLORS="no=00:fi=00:di=04;34:ln=01;36:pi=40;33:so=40;33:bd=40;33:cd=40;33:ex=01;31:or=04;36"
export PAGER="less"
export LESS="--raw-control-chars"

path=($HOME/bin(N-/) $HOME/.cabal/bin(N-/) $path)

[ -f ~/.zshenv.local ] && source ~/.zshenv.local

if which vim > /dev/null ; then
    EDITOR=vim
fi

coins=coins.tsukuba.ac.jp
