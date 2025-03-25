command -v brew || brew=false
if [ $brew == false ] ; then
    echo '======= Installing brew'
    run '/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"'
    run 'brew tap homebrew/homebrew-cask-font git@github.com:Homebrew/homebrew-cask-fonts.git'
    run 'brew tap homebrew/homebrew-cask git@github.com:Homebrew/homebrew-cask.git'
    run 'brew analytics off'
    run 'brew update'
else
    echo '======= Brew already installed'
fi

source $scriptPath/scripts/checkbox_input.sh

packages=(
    git-annex      # advanced alternative to git-lfs
)
checkbox_input "[Big Packages] Which packages would you like installed?" packages selected_packages_big

packages=(
    amethyst            # tiled window management
    firefox
    font-hack-nerd-font # fonts with icons
    google-chrome       # google's version of chrome
    iterm2              # terminal
    mpv                 # powerful media player
    virtualbox          # vm manager
                        # virtualbox-extension-pack # vm manager, broken
)
checkbox_input "[Cask Packages] Which packages would you like installed?" packages selected_packages_cask

packages=(
    # autojump       # j command, for jumping to recently used directories
    coreutils
    # docker
    # docker-compose # python wrapper for docker startup scripts
    # docker-machine # docker virtual machine management for mac
    emacs
    # fasd           # file searcher, https:                                / /github.com /clvv /fasd
    fpp            # for selecting files to open from command output
    fzf            # fuzzy searcher, dependency for other tools
    git
    git-lfs        # large file support, for tracking large files w /o adding to repo size uncessesarily
    hr             # inserts a horizonal line for scrollback
    hub            # scripts for interacting with github
    jq
    mosh
    neovim
    nnn            # command line file explorer
    node           # js based engine and package manager
    pidof          # linux program replacement
    pv             # pipe monitor
    ranger         # cml dir navigation
    ripgrep        # fast searcher, bound to rg
    rsync
    stow           # dotfile management
    tmux           # updated version of screens
    trash          # rm but integrated with finder's trash
    tree           # recursive ls with formatting
    wget
    yarn           # alternative to npm, node's package manager
    zplug          # zsh package manager
)
run "brew install $(join_by ' ' ${packages[@]})"

if [ "$(join_by ' ' ${selected_packages_big[@]})" == '' ] ; then
    echo '======== Nothing to install'
else
    run "brew install $(join_by ' ' ${selected_packages_big[@]})"
fi

if [ "$(join_by ' ' ${selected_packages_cask[@]})" == '' ] ; then
    echo '======== Nothing to install'
else
    run "brew cask install $(join_by ' ' ${selected_packages_cask[@]})"
fi

# vim plugins
[ -d ~/.vim/plugged ] || run 'curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
vim +PlugInstalll +qall

# checkbox_input "[Small Packages] Which packages would you like installed?" packages selected_packages
# if [ "$(join_by ' ' ${selected_packages[@]})" == '' ] ; then
#     echo '======== Nothing to install'
# else
#     run "brew install $(join_by ' ' ${selected_packages[@]})"
# fi

