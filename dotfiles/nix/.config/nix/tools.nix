{ pkgs, unstable-pkgs, ... }:
let
  ttp = pkgs.writeScriptBin "ttp" ''
    #!/usr/bin/env sh
    "$HOME/repos/tennysontbardwell/tennyson.ts/bin/index.cjs" "$@"
    '';
  tt = pkgs.writeScriptBin "tt" ''
    #!/usr/bin/env sh
    "$HOME/repos/tennysontbardwell/misc-projects/personal.ts/bin/index.js" "$@"
    '';
in
{
  linux_paths = with pkgs; [
    dmenu-rs
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
  ];

  paths = with pkgs; [

    #tmp
    pqiv
    firefox
    web-ext
    sops
    age

    #### general tools

    # editors / top
    tmux
    neovim
    emacs
    vim
    zsh
    pure-prompt

    # editors dependencies / assister-s
    pandoc
    texlive.combined.scheme-full
    ispell
    glibtool # vterm in emacs

    # searching / navigating
    ripgrep
    fzf

    # system info
    ncdu
    htop
    btop

    # network tools
    tailscale
    rsync
    wget
    socat
    curl
    tshark
    termshark

    # data format transformers
    jq
    yq-go
    xq-xml
    xan
    qsv
    html-tidy
    xidel

    # utility functions
    pv
    parallel
    pass

    # vc
    git
    git-lfs

    # media
    ffmpeg
    imagemagick
    graphviz
    mpv

    # misc
    nixos-anywhere
    irssi
    tty-share
    sioyek
    plantuml
    graph-easy

    #### VM stacks
    qemu
    minikube
    kubectl
    podman
    podman-tui
    kubernetes-helm


    #### GNU
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

    #### EMACs?
    # zotero-translation-server

    # maybe keep?
    gspell
    vdirsyncer
    libavif
    pkg-config
    rclone
    fpp # file selector
    sox # audio
    websocat
    yazi
    lf
    imgcat

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

    #### compression & data
    zstd
    xz
    brotli
    parquet-tools
    hdf5
    zip

    #### software stacks
    # java
    zulu
    zulu17
    maven
    # web
    nodejs
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

    ## text
    cowsay
    figlet
    lolcat
    # ponysay

    ## misc

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
