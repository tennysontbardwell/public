# Preferred editor for local and remote sessions
export EDITOR="nvim"
export VISUAL="nvim"
# export VISUAL="$HOME/.config/zsh/scripts/emc.sh"

# Go
export GOPATH=$HOME/golang
export PATH=$PATH:$GOPATH/bin

# Ruby
if [ -d $HOME/.gem/ruby ]; then
    _ruby_dir=$(ls $HOME/.gem/ruby | tail -n 1)
    if [ ! -z $_ruby_dir ]; then
        export PATH=$PATH:$HOME/.gem/ruby/$_ruby_dir/bin
    fi
fi

# ==== PROMPT ====

ZSH_THEME_VIRTUAL_ENV_PROMPT_PREFIX="("
ZSH_THEME_VIRTUAL_ENV_PROMPT_SUFFIX=") "

# misc ########################################################################
zstyle ':completion:*' menu select

if [[ $TERM = "dumb" ]]; then
    bash && exit
fi

if [[ $SSH_CONNECTION != "" && $TMUX = "" && $AUTO_TMUX_ON_SSH = 1 ]]; then
    declare SSH_TMUX_TARGET="${SSH_TMUX_TARGET:-ssh}"
    tmux has-session -t "$SSH_TMUX_TARGET" 2>/dev/null
    if [ "$?" -eq 1 ] ; then
        tmux new-session -d -s "$SSH_TMUX_TARGET"
    fi
    tmux attach-session -t "$SSH_TMUX_TARGET"
    echo -n "Do you wish to exit $HOST now? [Yn]"
    read yn
    case $yn in
        [Yy]* ) exit;;
        ""    ) exit;;
        [Nn]* ) : ;;
        *     ) echo "assuming no";;
    esac
fi
