# aliases #####################################################################
alias vnice=tbardwell__vnice
alias cd\.\.='tennyson_cd_up'
alias cl="cls -l"
alias cls="colorls -A"
# alias do_not_disturb="kill -SIGUSR1 $(pidof dunst)"
# alias do_not_disturb_off="kill -SIGUSR2 $(pidof dunst)"
alias ggcheck='gitcheck --maxdepth=2 --quiet --dir=$HOME -a -u' # list dirty git dir
alias ggwip='gst && (gwip || :) && ggpnp'
alias new='bbg terminator --working-dir=$PWD' # new terminal same dir
alias priv='HISTFILE_BK=HISTFILE && unset HISTFILE' # History management
alias r="ranger-cd"
alias skvir="source /usr/bin/virtualenvwrapper.sh" # loading python virtualenvwrapper
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

function gist() { pbpaste | gh gist create | pbcopy; }
archive() { _archive_loc=$(dirname $1)/archive  && mkdir -p $_archive_loc && mv -i $1 $_archive_loc }
bbg() { nohup "$@" &> /dev/null & } # runs something in bg, also hides output
dockerkill() {for a in $(docker ps | cut -f 1 -d ' ' | tail -n +2); do docker update --restart=no $a; done}
hhead() { head -$(expr $(tput lines) - 2) } # head which fits window size
# jjq() { jq $2 $1 } # jq but a prompt
# Return to last directory, see rebind commands, chpwd function
rcd() {cd "$(cat /tmp/.tennyson_zsh_last_dir_visited)"}
take() { mkdir -p $@ && cd ${@:$#} }
tennyson_cd_up() { cd $(printf "%0.0s../" $(seq 1 $1)); } # moving up directories

# big functions ###############################################################

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
function ranger-widget {
    ranger-cd
    zle reset-prompt
}
# key commands ################################################################

# This binds Ctrl-O to ranger-cd:
zle -N ranger-widget ranger-widget
bindkey "\C-o" ranger-widget
bindkey -r "^Os" # prevent delay in opening

# fixes ctrl-q to pushline
unsetopt flow_control

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

# new or temp #################################################################

function fork () {
    nohup "$@" > /dev/null 2>&1 &
    disown
}

function rmtmp {
    dir="$(echo $PWD | sed -ne "s|^\\($HOME/tmp/[^/]\\+\\).*|\\1|p")"
    [[ $dir = *[![:space:]]* ]] && cd $HOME && echo rm -rf $dir && rm -rf $dir
}

function eff {
    dir="$(cat ~/.config/tennyson/bookmarks.txt | fzf)"
    if [ ! -z "$dir" ]; then
       dir="$(eval echo -e "$dir")"
        emc "$dir"
    fi
}

function tt {
    declare -r nodepath="$HOME/projects/tbardwell.ts/build"
    NODE_PATH="$nodepath" node "$nodepath"/src/index.js
}

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

function tbardwell__ls_full_path() {
    find $PWD -maxdepth 1
}

function jjq() {
    declare file="$1" query="$2"
    printf "%'d / %'d\n" "$(cat "$file" | jq "$query" | wc -l)" "$(cat "$file" | jq "." | wc -l)"
    cat "$file" | jq -C "$query" | head -n 20
}

function ddocker() {
    declare image="${1:-}"
    if [ -z "$image" ]; then
        image="$(docker images | sed '1d' | cut -d ' ' -f 1 | sort -u | fzf)"
    fi
    [ -z "$image" ] && return 1
    docker run -it --rm "$image" bash
}

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

function ts () { 
    ( cd ~/repos/tennysontbardwell/tennyson.ts > /dev/null;
      yarn run run "$@")
}

function tp() {
    export BASH_EVAL_FILE="$(mktemp)"
    (cd ~/repos/tennysontbardwell/tennyson.ts/build; NODE_PATH=. node --enable-source-maps ./src/app/scripts/hometty.js < /dev/tty)
    [ ! 0 -eq "$(wc  -c "$BASH_EVAL_FILE" | sed -e 's/^ *\([0-9]\+\) .*/\1/' | tr -d '\n')" ] \
        && eval "$(cat "$BASH_EVAL_FILE")"
    rm "$BASH_EVAL_FILE"
}

function loop_tp() {
    while :
    do
        tp
        osascript -e 'tell application "Finder"' -e 'set visible of process "iTerm2" to false' -e 'end tell'
    done
}

function tp_widget() {
    export BASH_EVAL_FILE="$(mktemp)"
    (cd ~/repos/tennysontbardwell/tennyson.ts/build; NODE_PATH=. node --enable-source-maps ./src/app/scripts/hometty.js < /dev/tty)
    [ ! 0 -eq "$(wc -c "$BASH_EVAL_FILE" | sed -e 's/^ *\([0-9]\+\) .*/\1/' | tr -d '\n')" ] \
        && eval "$(cat "$BASH_EVAL_FILE")"
    rm "$BASH_EVAL_FILE"
    zle reset-prompt
}

zle     -N            tp_widget
bindkey -M emacs '^Y' tp_widget
bindkey -M vicmd '^Y' tp_widget
bindkey -M viins '^Y' tp_widget
