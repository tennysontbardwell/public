#!/usr/bin/env bash
IFS=$'\n\t'

declare -r arg1="${1:-}"

if [ "$arg1" = "-a" ]; then
    INSTALL_ALL=true
else
    INSTALL_ALL=false
    scriptPath=${0%/*}
    source $scriptPath/scripts/checkbox_input.sh
fi

function run() { echo '=======' $1; eval $1; }
function join_by { local d=$1; shift; echo -n "$1"; shift; printf "%s" "${@/#/$d}"; }
prompt_and_append() { # prompt prompt_and_append package_list list_to_append_to
    eval to_append=( '"${'${3}'[@]}"' )
    if $INSTALL_ALL ; then
        new_list=( "${to_append[@]}" "${package_list[@]}" )
    else
        checkbox_input "$1" "$2" selected
        new_list=( "${to_append[@]}" "${selected[@]}" )
    fi
    echo "${new_list[@]}"
    eval "$3="'"${new_list[@]}"'
}

run "sudo pacman -Syu"
run "sudo pacman -S --needed --noconfirm bc"
packages=(
    ack # stream editor
    alsa-utils
    aspell-en # dictionary for english
    aws-cli
    base
    base-devel
    bc
    bzip2
    curl
    dmenu # simple gui menu tool
    dnsutils
    emacs
    expac
    fzf # interactive fuzzy finder
    git
    hub
    iftop # real time network monitor, ip by ip
    inotify-tools
    jq
    lsof
    make
    ncdu
    neovim
    networkmanager
    nodejs
    npm
    pacman-mirrorlist # used for ranking pacman mirrors
    perl
    prettier
    pv # cat file | pv > file2 # used for showing amount of data flowing through pipe in shell
    pygmentize
    python
    python-beautifulsoup4
    python-boto3 # for aws in tennyson.py
    python-colorama # for aws-cli
    python-numpy
    python-pandas
    python-pip
    python-scipy
    python-seaborn
    python-lxml
    python2
    ranger # visual cd
    reflector
    rsync
    ruby
    stow
    strace # system call trace util
    tar
    tmux
    trash-cli # provides the `trash` command
    tree
    unzip
    vim
    wget
    xdotool # use `xdotool type $TEXT` to simulate typing
    yajl
    yarn
    zip
    zsh
)
to_install_packages="${packages[@]}"


packages=(
    adobe-source-code-pro-fonts    # programer font
    adobe-source-han-sans-jp-fonts # japanese font
    darktables                     # for editing/converting RAW photos
    dolphin                        # decent file broswer
    dunst                          # lightweight notification server
    firefox
    gnome-screenshot               # screenshot tool, does not require gnome
    handbrake                      # video conversion tool
    handbrake-cli                  # video conversion tool cli
    i3
    mpv                            # media player
    ncdu                           # du on steroids
    nm-applet                      # NetworkManger gui & dock icon
    openssh
    rxvt-unicode                   # Terminal
    synapse                        # app launcher
    sxhkd
    thunar                         # lighweight file browser
    vlc                            # media player
    xorg
)
prompt_and_append "[GUI Packages] Which packages would you like installed?" packages to_install_packages


packages=(
    docker
    docker-compose
    git-annex
    grml-zsh-config # simple zsh config
    mosh      # better alternative to ssh
    openssh
    sysstat
    texlive-core
    texlive-latexextra
    ufw       # simple port/firewall management
    virtualbox
    virtualbox-guest-utils # virtual box guest addition with x support
    wifi-menu # easy, interactive wifi terminal interface
    xf86-video-vmware # virtual box guest addition with x support
)
prompt_and_append "[Development Packages] Which packages would you like installed?" packages to_install_packages


packages=(
    acpi # battery status indicator
    tlc
)
prompt_and_append "[Laptop Packages] Which packages would you like installed?" packages to_install_packages


packages=(
    udisks2
    udiskie
)
prompt_and_append "[Optional Computer Packages] Which packages would you like installed?" packages to_install_packages


packages=(
    autojump
    argbash      # code generator for bash arg parsing
    entr         # filesystem watcher, example: `find . -name '*.py' | entr ./myfile.py`
    fzy
    gitcheck-git # multi git repo checker
    nnn          # command line file explorer
    parallel     # gnu parallel
    percol
    ripgrep      # fast searcher, bound to rg
    zplug        # zsh package manager
    zsh-pure-prompt
)
to_install_aur_packages="${packages[@]}"

packages=(
    dropbox             # daemon and tray icon
    freeoffice          # Microsoft office alternative
    google-chrome
    google-chrome-dev
    nerd-fonts-complete # fonts with icons
    ttf-monaco          # apple's monaco font (a code font)
    unison              # file syncer
)
prompt_and_append "[Optional User Packages] Which packages would you like installed?" packages to_install_aur_packages


run "sudo pacman -S --needed --noconfirm $(join_by ' ' ${to_install_packages[@]})"
run "sudo pacman -S --noconfirm $(join_by ' ' ${to_install_packages[@]})"

run "sudo pacman -Syu --noconfirm"

if [ ! -n "$(pacman -Qs yay)" ]; then
    echo "======== Install "yay" from AUR"
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    echo asdf
    sudo ls
    makepkg -si
    # makepkg PKGBUILD --skippgpcheck --install --needed
    # sudo pacman -U --noconfirm yay*xz
else
    echo "======== "yay" already installed"
fi

run "yay -Syu --noconfirm"
run "yay -S --noconfirm $(join_by ' ' ${to_install_aur_packages[@]})"

