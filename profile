
export PATH="$HOME/.local/bin:${PATH}"


source_if_exists(){
    if [[ -e $1 ]]; then
        . "$1"
    fi
}

source_if_exists ~/.localprofile
source_if_exists ~/.startagent
source_if_exists ~/.gitcompletion
source_if_exists ~/.stackcompletion

source_if_exists ~/.bashrc

case $(uname) in
    "MINGW64_NT-10.0")
        export EDITOR='emacsclient -na "runemacs" -c' ;;
    *)
        export EDITOR='emacsclient -t -a ""'
        export VISUAL='emacsclient -c -a ""';;
esac
