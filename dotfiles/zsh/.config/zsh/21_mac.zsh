if [[ `uname` == 'Darwin' ]]; then
    alias consul=/Applications/consul
    alias docker=podman
    PATH=$PATH:/opt/homebrew/bin

    function notify() {
        osascript -e "display notification \"${2:-nothing}\" with title \"$1\""
    }
elif [[ `uname` == 'Linux' ]]; then
    :
fi
