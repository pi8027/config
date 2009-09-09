
# env
export LANG=ja_JP.UTF-8
export PATH="$PATH:$HOME/bin/"

# prompt

PROMPT="%/%% "
PROMPT2="%_%% "
SPROMPT="%r is correct? [n,y,a,e]: "

# history

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history
function history-all
{
	history -E 1
}

# comp

autoload -U compinit
compinit
zstyle ':completion:*' list-colors ''

# ls

alias ls="ls --color=auto"

# with GNU screen

if [ $TERM = "screen" ]; then
	preexec(){
		echo -ne "\ek`pwd | sed -e 's/^\/home\/pi8027/~/g'`% $1\e\\"
	}
else
	screen
fi
