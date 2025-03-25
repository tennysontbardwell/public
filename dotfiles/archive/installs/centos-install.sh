dnf_packages=(
    rsync
    htop
    openssh
    krb5-libs
    krb5-workstation
    ca-certificates
    git
    emacs
    vim
    neovim
    zsh
    bind-utils
    iftop # Internet interFace TOP
    jq
    inotify-tools # fs watching
    make
    pv
    perl
    stow
    tmux
    wget
    strace
    tar
    zip
    tree
    bzip2
    ncdu
    nodejs
    ruby
    aspell-en
    python3
    python3-pip
    unzip
    entr         # filesystem watcher, example: `find . -name '*.py' | entr ./myfile.py`
    # qemu-kvm
    # gitlab-runner
    # docker-ce
    # docker-ce-cli
    # containerd.io
    # tigervnc
    # firefox
    # dnf
    # net-tools
    # i3
    # nfs-utils
)

dnf install "${dnf_packages[@]}"

# fzf
# ranger
# autojump

npm_packages=(
    pure-prompt
    yarn
)

npm install -g "${npm_pakcages[@]}"

