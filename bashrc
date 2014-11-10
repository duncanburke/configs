# Check that gpg-agent is running, regardless of interactive status
. ~/.start_agent

. ~/ghc_scripts

## Check for an interactive session
[ -z "$PS1" ] && return

# colored prompt
if [ "`tput colors`" = "256" ]; then
  B="\e[0;38;5;67m"
  G="\e[0;38;5;114m"
  Y="\e[0;38;5;214m"
else
  B="\e[0;34m"
  G="\e[0;32m"
  Y="\e[0;33m"
fi

W="\e[0m"
PS1="\[$B\]┌─\[$W\][ \[$Y\]\A \[$W\]][ \[$G\]\h:\w \[$W\]]\n\[$B\]└─\[$Y\]> \[$W\]"

alias nemacs='emacs -nw'
alias ls='ls -h --color=auto'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'                    # 'rm -i' prompts for every file
alias ln='ln -i'

complete -cf sudo
complete -cf man
set show-all-if-ambiguous on
set show-all-if-unmodified on

export MPD_HOST='/var/lib/mpd/socket'

export PATH='.cabal-sandbox/bin:~/.cabal/bin':$PATH

source ~/.git-completion.sh

export PRINTER='xerox_3220'
export EDITOR="emacs"

# As this in an interactive shell, set GPG_TTY
export GPG_TTY=`tty`
