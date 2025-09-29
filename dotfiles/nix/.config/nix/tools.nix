{ pkgs, ... }:
let
  ttp = pkgs.writeScriptBin "ttp" ''
    #!/usr/bin/env sh
    bun run "$HOME/repos/tennysontbardwell/tennyson.ts/tennyson/index.ts" -- "$@"
    # "$HOME/repos/tennysontbardwell/tennyson.ts/bin/index.cjs" "$@"
    '';
  tt = pkgs.writeScriptBin "tt" ''
    #!/usr/bin/env sh
    bun run "$HOME/repos/tennysontbardwell/misc-projects/personal.ts/tennyson-personal/bin/tt.ts" -- "$@"
    # "$HOME/repos/tennysontbardwell/misc-projects/personal.ts/bin/index.js" "$@"
    '';
in
{
  linux_paths = with pkgs; [
    # build issue on 2025-09-15
    # dmenu-rs
    kdePackages.dolphin
    gnome-screenshot
    i3
    rxvt-unicode
    synapse
    xfce.thunar
    # wifi-menu
    sysstat
    xclip
    kubernetes
    bubblewrap
  ];

  paths = with pkgs; [
    #tmp
    firefox
    firefox-devedition
    web-ext
    sops
    age

    ### editors / text
    neovim
    emacs
    vim
    #### emacs dependencies
    ispell
    glibtool # for vterm in emacs
    delta
    # zotero-translation-server
    #### latex
    pandoc
    texlive.combined.scheme-full
    #### ascii graphs
    plantuml
    graph-easy

    ### shells
    zsh
    tmux
    pure-prompt

    ### searching / navigating
    ripgrep
    fzf

    ### system info
    ncdu
    htop
    btop

    ### network tools
    #### transfer
    rsync
    wget
    socat
    curl
    rclone
    #### vpn
    tailscale
    #### inspection
    tshark
    termshark

    ### data format transformers
    jq
    yq-go
    xq-xml
    xan
    qsv
    html-tidy
    xidel

    ### utility functions
    pv
    parallel
    pass

    ### vc
    git
    git-lfs

    ### media
    #### video
    ffmpeg
    mpv
    #### img
    imagemagick
    graphviz
    pqiv
    ### pdf
    sioyek
    #### games
    ruffle

    ### social
    irssi
    tty-share

    ### nixos
    nixos-anywhere

    ### VM stacks
    qemu
    minikube
    kubectl
    podman
    podman-tui
    kubernetes-helm
    cri-tools
    k9s
    cloudlens

    ### GNU
    coreutils-full
    findutils
    gnugrep
    gnupg
    gnumake
    cmake
    automake
    autoconf
    gawk
    pwgen

    # maybe keep?
    gspell
    vdirsyncer
    libavif
    pkg-config
    fpp # file selector
    sox # audio
    websocat
    yazi
    lf
    imgcat
    mc

    #### mac specific CLI tool overrides
    gnused
    gnutar
    perl

    #### dependencies for tools
    git-remote-gcrypt
    inkscape
    cairo
    nixpkgs-fmt
    nixfmt-rfc-style

    #### my favorites tools
    ranger
    stow
    # zplug
    hr
    watch
    mosh
    tree
    git-annex
    borgbackup
    borgmatic
    awscli
    visidata
    lnav
    rclone
    yt-dlp
    gallery-dl
    jless
    ledger
    graphviz
    gh # github
    ollama
    tenv

    # trying out
    pls
    # prettier?

    ### compression & data
    zstd
    xz
    brotli
    parquet-tools
    hdf5
    zip

    ### software stacks
    #### java
    zulu
    zulu17
    maven
    #### web
    nodejs
    nodePackages.prettier
    bun
    yarn
    php
    go
    typescript

    #### fun random things

    ## animations
    cmatrix
    # globe-cli
    # sl
    # cbonsai

    ## activity
    # hollywood
    # genact

    ### fun TUI text
    cowsay
    figlet
    lolcat
    # ponysay

    #### tried out in the past - CLI tools
    # openai-whisper =
    # kubernetes-cli =
    # hub            = github
    # khard          = contact cards
    # qpdf           = pdfs
    # gb             = images
    # wimlib         = images
    # pdftk-java     = pdf lib
    # duti           = default app settings
    # inotify-tools
    # autojump
    # entr           = filesystem watcher, example: `find . -name '*.py' | entr ./myfile.py`
    # gitcheck-git   = multi git repo checker
    # percol
    # nnn            = file browser in cli

    # custom
    tt
    ttp
  ];
}
