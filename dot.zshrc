
source ~/.zshrc.local

# prompt

autoload colors
colors

if [ $TERM != "dumb" ]; then
	PROMPT="%U%{${fg[red]}%}[%n@%M]%{${reset_color}%}%u(%j) %~
%# "
	PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
	SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
else
	PROMPT="%U[%n@%M]%u(%j) %~
%# "
	PROMPT2="%_%% "
	SPROMPT="%r is correct? [n,y,a,e]: "
	[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] && 
		PROMPT="$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
fi

# history

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history
setopt share_history
setopt hist_ignore_all_dups

# comp

autoload -Uz compinit ;compinit
zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
zstyle ":completion:*:sudo:*" command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
LISTMAX=0

# alias

alias history-all="history -E 1"
alias ls="ls -G -p"
alias less="less --quit-if-one-screen --raw-control-chars"
alias emacs="emacs --no-window-system"
alias gosh="rlwrap gosh"
alias maxima="rlwrap maxima"
alias coqtop="rlwrap coqtop"
alias gs="rlwrap gs"
alias ghc="ghc --make"
alias hoogle="hoogle --color=true"
alias google="w3m http://google.com/"
alias cabal-install="sudo cabal install --global"

# screen

#if [ -z $STY ]; then
#	screen -R
#else
#	preexec(){
#		echo -ne "\ek`pwd | sed -e "s/^\/Users\/pi8027/~/g"`% $1\e\\"
#	}
#fi

# tmux

if [ -z $TMUX ] && [ -z $WITHOUT_SCREEN ] && [ $TERM != "screen" ]; then
	tmux -u
	export WITHOUT_SCREEN=1
fi


