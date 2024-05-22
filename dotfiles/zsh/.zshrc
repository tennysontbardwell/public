source_if_exists() {
    if [ -f $1 ]; then
        source $1
    fi
}

source_if_exists ~/.zshrc.local
source_if_exists ~/.zshrc.local.zsh
source_if_exists ~/.config/zsh.personal/hosts/$HOST.zsh

PATH=$PATH:~/.local/bin

function tbardwell__loadfuncs {
    for f in $(find $HOME/.config/zsh/ $HOME/.config/zsh.personal/ -maxdepth 1 -name "*.zsh" | sort); do
        source "$f"
    done
}
tbardwell__loadfuncs
