
# env
export LANG=ja_JP.UTF-8
export PATH="$HOME/bin:$PATH"

# prompt

autoload colors
colors

PROMPT="%{${fg[red]}%}${USER}@${HOST} : %/%%%{${reset_color}%} "
PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"

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
alias less="less --quit-if-one-screen --RAW-CONTROL-CHARS"
alias gosh="rlwrap -break-chars '(){}[],#\";| ' -f $HOME/.rlwrap/gosh_completions gosh"
alias emacs="emacs --no-window-system"

# with GNU screen

if [ $TERM[1,6] = "screen" ]; then
	preexec(){
		echo -ne "\ek`pwd | sed -e 's/^\/home\/pi8027/~/g'`% $1\e\\"
	}
else
	screen
fi
