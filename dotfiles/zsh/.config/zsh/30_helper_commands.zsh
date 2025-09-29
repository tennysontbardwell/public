# aliases #####################################################################
alias vnice=tbardwell__vnice
alias ggwip='gst && (gwip || :) && ggpnp'
alias new='bbg terminator --working-dir=$PWD' # new terminal same dir
alias priv='HISTFILE_BK=HISTFILE && unset HISTFILE' # History management
# alias r="ranger-cd"
alias r="yazi-cd"
alias tree1="tree -L 1"
alias tree2="tree -L 2"
alias tree3="tree -L 3"
alias tree4="tree -L 4"
alias ccal='ncal -3b'
alias pm="podman"
alias pmm="podman machine"
alias pmmi="podman machine info"
alias pmmu="podman machine start"
alias pmmd="podman machine stop"
alias ttmp='cd `mktemp -d`'
alias tttmp='take $HOME/tmp/$(date +%Y-%m-%d--%H%M%S)'
alias unpushed='git log --branches --not --remotes --no-walk --decorate --oneline'

# small functions #############################################################

archive() { _archive_loc=$(dirname $1)/archive  && mkdir -p $_archive_loc && mv -i $1 $_archive_loc }
bbg() { nohup "$@" &>/dev/null & } # runs something in bg, also hides output
dockerkill() {for a in $(docker ps | cut -f 1 -d ' ' | tail -n +2); do docker update --restart=no $a; done}
fork () { nohup "$@" > /dev/null 2>&1 &; disown } # and disown
gist() { pbpaste | gh gist create | pbcopy; }
hhead() { head -$(expr $(tput lines) - 2) } # head which fits window size
take() { mkdir -p $@ && cd ${@:$#} }

# big functions ###############################################################

function ff {
    dir="$(cat ~/.config/tennyson/bookmarks.txt | fzf)"
    dir="$(eval echo -e "$dir")"
    if [ -f "$dir" ]
    then
       vim "$dir"
    else
       ranger-cd "$dir"
    fi
}

function ddocker() {
    declare image="${1:-}"
    if [ -z "$image" ]; then
        image="$(docker images | sed '1d' | cut -d ' ' -f 1 | sort -u | fzf)"
    fi
    [ -z "$image" ] && return 1
    docker run -it --rm "$image" bash
}

# widgets ###############################################################
# https://github.com/ranger/ranger/blob/master/examples/bash_automatic_cd.sh#L10
function ranger-cd {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    ranger --choosedir="$tempfile" "${@:-$(pwd)}" < $TTY
    test -f "$tempfile" &&
        if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
            cd -- "$(cat "$tempfile")"
        fi
    rm -f -- "$tempfile"
}

function yazi-cd {
    tempfile="$(mktemp -t tmp.XXXXXX)"
    yazi --cwd-file="$tempfile" "${@:-$(pwd)}" < $TTY
    test -f "$tempfile" &&
        if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
            cd -- "$(cat "$tempfile")"
        fi
    rm -f -- "$tempfile"
}

function ranger-widget {
    # ranger-cd
    yazi-cd
    zle reset-prompt
}

# This binds Ctrl-O to ranger-cd:
zle -N ranger-widget ranger-widget
bindkey "\C-o" ranger-widget
bindkey -r "^Os" # prevent delay in opening

# key commands ################################################################
unsetopt flow_control # fixes ctrl-q to pushline

# platform specific ###########################################################

function tbardwell__vnice() {
    if [ `uname` = "Linux" ]; then
        ionice -c2 -n7 nice -n 19 "$@" # lowering task priority for batch jobs
    elif [ `uname` = "Darwin" ]; then
        nice -n 19 "$@" # lowering task priority for batch jobs
    fi
}


if [[ $(uname) == 'Linux' ]]; then
    alias pbcopy='xclip -selection clipboard'
    alias pbpaste='xclip -selection clipboard -o'
    open() {
        if [ $# -eq 0 ]; then
          bbg xdg-open . &> /dev/null
        else
          for file in "$@"; do
            bbg xdg-open "$file" &> /dev/null
          done
        fi
    }
fi

# hr ##########################################################################
hr() {
    local COLS="$(tput cols)"
    if (( COLS <= 0 )) ; then
        COLS="${COLUMNS:-80}"
    fi

    local WORD="${1:-#}"
    local LINE=''

    if [[ -z "$WORD" ]] ; then
        return;
    fi

    printf -v LINE '%*s' "$COLS"
    LINE="${LINE// /${WORD}}"
    echo "${LINE:0:${COLS}}"
}

hhr() {
    hr
    printf "# "
    echo "$@"
    hr
}

hrs() {
    local WORD

    for WORD in "${@:-#}"
    do
        hr "$WORD"
    done
}

# completion ##################################################################
if command -v aws >/dev/null 2>&1 && command -v aws_completer >/dev/null 2>&1; then
    complete -C "$(command -v aws_completer)" aws
fi

# tt and tennyson.ts ##########################################################
_tt_yargs_completions()
{
    local reply
    local si=$IFS
    IFS=$'
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" tt --get-yargs-completions "${words[@]}"))
    IFS=$si
    _describe 'values' reply
}
compdef _tt_yargs_completions tt
###-end-index.cjs-completions-###

function tt_widget() {
    export BASH_EVAL_FILE="$(mktemp)"
    tt hometty < /dev/tty
    [ ! 0 -eq "$(wc -c "$BASH_EVAL_FILE" | sed -e 's/^ *\([0-9]\+\) .*/\1/' | tr -d '\n')" ] \
        && eval "$(cat "$BASH_EVAL_FILE")"
    rm "$BASH_EVAL_FILE"
    zle reset-prompt
}

zle     -N            tt_widget
bindkey -M emacs '^Y' tt_widget
bindkey -M vicmd '^Y' tt_widget
bindkey -M viins '^Y' tt_widget
