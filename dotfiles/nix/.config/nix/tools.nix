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
    # custom ##################################################################
    tt
    ttp

    # tmp / misc ##############################################################
    #### dependencies
    pkg-config # not sure why I need this
    cairo # dependency for some tool
    #### to disable then del
    # hr
    #### ranger alt
    # lf
    #### unsorted for now
    lnav
    ledger
    ollama

    # editors / text ##########################################################
    neovim
    emacs
    vim
    #### spelling
    ispell
    gspell
    #### emacs dependencies
    glibtool # for vterm in emacs
    delta
    # zotero-translation-server
    #### latex
    pandoc
    texlive.combined.scheme-full
    #### ascii graphs
    plantuml
    graph-easy

    # shells ##################################################################
    zsh
    tmux
    #### shell packages
    pure-prompt
    # zplug
    #### files / dir
    ranger
    yazi
    pls
    tree
    #### command modifiers / utility functions
    watch
    pv
    parallel
    #### searching / navigating
    ripgrep
    fzf
    fpp # file selector

    # system info #############################################################
    ncdu
    htop
    btop

    # network tools ###########################################################
    #### transfer
    rsync
    wget
    socat
    curl
    rclone
    websocat
    #### vpn
    tailscale
    #### inspection
    tshark
    termshark

    # data ####################################################################
    #### viz
    visidata
    jless
    #### transformers
    jq
    yq-go
    xq-xml
    xan
    qsv
    html-tidy
    xidel

    # vc ######################################################################
    git
    git-lfs
    git-annex
    git-remote-gcrypt
    #### configs
    stow
    #### api
    gh # github

    ### media
    #### video
    ffmpeg
    mpv
    libavif
    #### img
    imagemagick
    graphviz
    pqiv
    inkscape
    #### audio
    sox
    ### pdf
    sioyek
    #### games
    ruffle
    #### download
    yt-dlp
    gallery-dl

    # social ##################################################################
    irssi
    tty-share

    # devops ##################################################################
    #### ssh
    mosh
    #### backup
    borgbackup
    borgmatic
    #### VM stacks
    qemu
    podman
    podman-tui
    #### k8
    minikube
    kubectl
    kubernetes-helm
    cri-tools
    k9s
    #### cloud
    cloudlens
    awscli2
    #### tf
    # nodePackages.cdktf-cli
    tenv
    #### nixos
    nixos-anywhere
    nixpkgs-fmt
    nixfmt-rfc-style
    #### secrets
    sops
    age
    pass

    # GUIs ####################################################################
    #### browsers
    firefox
    firefox-devedition
    web-ext

    # GNU #####################################################################
    autoconf
    automake
    cmake
    coreutils-full
    findutils
    gawk
    gnugrep
    gnumake
    gnupg
    pwgen
    util-linux

    # mac specific, override system default ###################################
    gnused
    gnutar
    perl

    # compression and data encodings ##########################################
    zstd
    xz
    brotli
    parquet-tools
    hdf5
    zip

    # software stacks #########################################################
    #### java
    zulu
    zulu17
    maven
    #### js runtime
    bun
    nodejs
    #### js pkg manger
    yarn
    pnpm
    #### js dev
    nodePackages.prettier
    typescript
    #### other
    go
    php

    # fun random things #######################################################
    #### animations
    cbonsai
    cmatrix
    globe-cli
    # sl
    #### activity
    # hollywood
    genact
    #### fun TUI text
    cowsay
    figlet
    lolcat

    # to del, old #############################################################
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
  ];
}
