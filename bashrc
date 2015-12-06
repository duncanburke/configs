# -*- mode: shell-script; -*-

# Check that gpg-agent is running, regardless of interactive status
if [[ -e ~/.start_agent ]]; then
    . ~/.start_agent
fi

## Check for an interactive session
[[ -z "$PS1" ]] && return

# colored prompt
if [ "`tput colors`" = "256" ]; then
    R="\e[0;38;5;125m"
    B="\e[0;38;5;67m"
    G="\e[0;38;5;114m"
    Y="\e[0;38;5;214m"
else
    R="\e[0;41m"
    B="\e[0;34m"
    G="\e[0;32m"
    Y="\e[0;33m"
fi

W="\e[0m"
[[ $EUID -ne 0 ]] && U_COL=$G || U_COL=$R
PS1="\[$B\]┌─\[$W\][ \[$Y\]\A \[$W\]][ \[$U_COL\]\u\[$G\]@\h:\w \[$W\]]\n\[$B\]└─\[$Y\]> \[$W\]"

alias nemacs='emacs -nw'
alias ls='ls -h --color=auto'
alias rm='rm -I'
alias ec='emacsclient -cnw -a ""'
alias ed='emacs -q --load "~/configs/emacs/init.el"'

complete -cf sudo
complete -cf man
set show-all-if-ambiguous on
set show-all-if-unmodified on

# As this in an interactive shell, set GPG_TTY
export GPG_TTY=`tty`

function hmap { ghc -e "interact ($*)";  }
function hmapl { hmap  "unlines.($*).lines" ; }
function hmapw { hmapl "map (unwords.($*).words)" ; }

if which stack > /dev/null; then
    eval "$(stack --bash-completion-script stack)"
fi

stty stop undef
stty start undef
