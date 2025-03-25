{ pkgs, ... }:
{
  paths = with pkgs; [
    #### general tools

    # editors / top
    tmux
    neovim
    vim

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

    # network tools
    rsync
    wget
    socat
    curl

    # data format transformers
    jq

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

    # misc
    nixos-anywhere

    #### VM stacks
    qemu
    minikube
    kubectl
    podman
    kubernetes-helm


    #### GNU
    coreutils-full
    findutils
    gnugrep
    gnupg
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

    #### mac specific CLI tool overrides
    gnused
    gnutar
    perl

    #### dependencies for tools
    git-remote-gcrypt

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
    rclone
    btop
    yt-dlp
    gallery-dl
    jless
    ledger
    graphviz
    gh # github
    ollama

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
    maven
    # web
    nodejs
    yarn
    php

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
  ];
}
