
# env

LANG=ja_JP.UTF-8
export LS_COLORS="no=00:fi=00:di=04;34:ln=01;36:pi=40;33:so=40;33:bd=40;33:cd=40;33:ex=01;31:or=04;36"

# prompt

autoload colors
colors

PROMPT="%U%{${fg[red]}%}[%n@%M]%{${reset_color}%}%u(%j) %~
%# "
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
alias ghc="ghc --make"
alias hoogle="hoogle --color=true"
alias starttig="mono ~/tig/Bin/TwitterIrcGatewayCLI.exe --encoding=utf-8"
alias stoptig="mono ~/tig/Bin/TwitterIrcGatewayCLI.exe --encoding=utf-8"
alias google="w3m http://google.com/"

# with uim-fep, screen

if [ -z $STY ]; then
	screen -R
else
	preexec(){
		echo -ne "\ek`pwd | sed -e "s/^\/Users\/pi8027/~/g"`% $1\e\\"
	}
fi

