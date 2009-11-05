
# env
export LANG=ja_JP.UTF-8
export PATH="$HOME/bin/:$PATH"

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

# alias

alias ls="ls --color=auto"
alias less="less -F -R"

# with GNU screen

if [ $TERM[1,6] = "screen" ]; then
	preexec(){
		echo -ne "\ek`pwd | sed -e 's/^\/home\/pi8027/~/g'`% $1\e\\"
	}
else
	screen
fi
