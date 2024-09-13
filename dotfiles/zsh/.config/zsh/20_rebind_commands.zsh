# Rebind Commands #############################################################
alias ag='ag -i'
alias df='df -h'
alias du="du -h"
# alias emacs="emacs -nw"
alias emc="emacsclient -nw -c"
alias -g G="| grep -i"
alias glo="git log --decorate=full --all --stat"
alias glog="git log --oneline --decorate --graph --all"
alias gt="git tag"
alias gta="git tag -a"
alias rmv="rsync -aP --remove-source-files "
alias rrsync="rsync -aP "
alias sudo="sudo " # sudo completion
alias tmux="tmux -2" # for tmux colors
alias view="nvim -R"
alias vim="nvim"
alias watch='watch -n 0.1 '

# ls ##########################################################################
# for the ruby colorls script
if [ -x "$(command -v colorls)" ]; then
    alias cl="cls -l"
    alias cls="colorls -A"
fi

if [[ `uname` == 'Darwin' ]]; then
    alias ls="ls -AhG"
elif [[ `uname` == 'Linux' ]]; then
    alias ls="ls -Ah --color=auto"
fi

# misc ########################################################################
# command to run each cd
# chpwd() {
    # ls
    # echo $PWD > /tmp/.tennyson_zsh_last_dir_visited
# }

# macos #######################################################################
if [[ `uname` == 'Darwin' ]]; then
    alias sed=gsed
fi
