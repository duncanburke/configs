export EDITOR="emacs -nw"
## Check for an interactive session
[ -z "$PS1" ] && return

#LC_ALL="en_AU.ISO-8859-1"
#export LANG="en_AU.ISO-8859-1"
alias ls='ls --color=auto'
alias pacman='sudo /usr/bin/pacman'
alias suspend='sudo pm-suspend'

txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

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

# modified commands
alias diff='colordiff'              # requires colordiff package
alias grep='grep --color=auto'
alias more='less'
alias df='df -h'
alias du='du -c -h'
alias mkdir='mkdir -p -v'
alias nano='nano -w'
alias ping='ping -c 5'
alias ..='cd ..'
alias nemacs='emacs -nw'

# new commands
alias da='date "+%A, %B %d, %Y [%T]"'
alias du1='du --max-depth=1'
alias hist='history | grep $1'      # requires an argument
alias openports='netstat --all --numeric --programs --inet --inet6'
alias pg='ps -Af | grep $1'         # requires an argument (note: /usr/bin/pg is installed by the util-linux package; maybe a different alias name should be used)
alias mplaylist='mplayer -loop 0 -shuffle -novideo -playlist'

# privileged access
if [ $UID -ne 0 ]; then
    alias sudo='sudo '
    alias scat='sudo cat'
    alias svim='sudo vim'
    alias semacs='sudo emacs'
    alias root='sudo su'
    alias reboot='sudo reboot'
    alias halt='sudo halt'
    alias update='sudo pacman -Su'
    alias netcfg='sudo netcfg2'
fi

# ls
alias ls='ls -hF --color=auto'
alias lr='ls -R'                    # recursive ls
alias ll='ls -l'
alias la='ll -A'
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date
alias lm='la | more'

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'



complete -cf sudo
complete -cf man
set show-all-if-ambiguous on
set show-all-if-unmodified on
export MPD_HOST='/home/duncan/mpd/socket'

export PATH=$PATH':/usr/bin/site_perl'
export PIP_INDEX_URL='http://pip'
export PACKAGE_HOST='http://packages'
export CONTEXT_HOST='http://context'

gateway="master01"

ssh_helper() {
       host=$1
       shift
       case "$host" in
               db*|spartan*|hydra*|charon*|monitor*|vnc*|phoebe*|artemis*|pandora*|orpheus*|apollo*|*charon*)
                               /usr/bin/ssh $gateway -t "ssh $host '$@'"
                       ;;
               *)
                               /usr/bin/ssh $host $@
                       ;;
       esac
}

alias ssh='ssh_helper'
