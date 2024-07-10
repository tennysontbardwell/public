if [[ `uname` == 'Darwin' ]]; then
    alias consul=/Applications/consul
    alias docker=podman
    PATH=$PATH:/opt/homebrew/bin
elif [[ `uname` == 'Linux' ]]; then
    :
fi
