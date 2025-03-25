#!/bin/bash

# zsh oh my zsh
if [ ! -d ~/.oh-my-zsh ]; then
    echo "Install Oh My Zsh"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

# requires user interaction
sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"

# git config
echo "Setting up Git Configs"
git config --global --add github.user tennysontbardwell
# git config --global user.email "REDACTED"
git config --global user.name "Tennyson T Bardwell"
git config --global init.defaultBranch "main"

########################################
# vim
########################################

# plug
if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ]; then
    echo "Installing Plug.vim"
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    nvim +PlugInstall +qall
fi

# spacemacs
if [ ! -f .spacemacs ]; then
    echo "Installing Spacemacs"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi
