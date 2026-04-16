{ pkgs, ... }:
let
  ttp = pkgs.writeScriptBin "ttp" ''
    #!/usr/bin/env dash
    bun run "$HOME/repos/tennysontbardwell/tennyson.ts/tennyson/index.ts" -- "$@"
  '';
  tt = pkgs.writeScriptBin "tt" ''
    #!/usr/bin/env dash
    bun run "$HOME/repos/tennysontbardwell/misc-projects/personal.ts/tennyson-personal/bin/tt.ts" -- "$@"
  '';
in
{
  linux_paths = with pkgs; [
    # build issue on 2025-09-15
    # dmenu-rs
    kdePackages.dolphin
    gnome-screenshot
    i3
    # rxvt-unicode # broken
    synapse
    xfce.thunar
    # wifi-menu
    sysstat
    xclip
    kubernetes
    bubblewrap
    # email
    postfix

    pinentry-all
    nftables
  ];

  paths = with pkgs; [
    # custom ##################################################################
    tt
    ttp

    # tmp / misc ##############################################################
    #### dependencies
    pkg-config # not sure why I need this
    cairo # dependency for some tool
    #### unsorted for now
    ledger
    (import ./imessage-exporter.nix { inherit pkgs; }).imessage-exporter
    bat
    highlight
    gdb
    tokei
    espanso
    kitty
    monolith
    #### email
    isync
    mailutils
    msmtp
    # mu
    # mutt
    notmuch

    # caldav ##################################################################
    khal
    khard
    vdirsyncer

    # editors / text ##########################################################
    neovim
    vim
    #### spelling
    ispell
    gspell
    # zotero-translation-server
    #### latex
    pandoc
    texlive.combined.scheme-full
    ghostscript
    typst
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
    jless
    otree
    visidata
    #### transformers
    jq
    yq-go
    xq-xml
    opensp # SGML
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

    # media ###################################################################
    #### video
    ffmpeg
    ffmpegthumbnailer
    mpv
    libavif
    #### img
    graphviz
    imagemagick
    # inkscape
    libpng
    pqiv
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
    # tty-share

    # devops ##################################################################
    #### ssh
    mosh
    #### backup
    borgbackup
    borgmatic
    #### VM stacks
    # packer # not free
    # lima
    podman
    podman-tui
    qemu
    #### k8
    cri-tools
    k9s
    kompose
    kubectl
    kubernetes-helm
    minikube
    talosctl
    #### cloud
    awscli2
    azure-cli
    cloudlens
    hcloud
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
    #### security
    trivy
    clamav

    # GUIs ####################################################################
    #### browsers
    # firefox
    # firefox-devedition
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
    openssl
    pkg-config
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
    # yarn
    yarn-berry
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
    # cmatrix
    # globe-cli
    # sl
    #### activity
    # hollywood
    # genact
    #### fun TUI text
    # cowsay
    # figlet
    # lolcat

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

    #### emacs dependencies
    (aspellWithDicts (dicts: with dicts; [ en ]))
    bash-language-server
    delta
    glibtool # for vterm in emacs
  ];
}
