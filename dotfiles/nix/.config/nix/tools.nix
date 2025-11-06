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
  my-emacs = pkgs.emacs.override {
    # withNS = false;
    withPgtk = true;
    # withXwidgets = true;
    withWebP = true;
    # withNativeCompilation = true;
    # withSQLite3 = true;
    # withTreeSitter = true;
  };
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
    (import ./imessage-exporter.nix { inherit pkgs; }).imessage-exporter
    bat
    highlight
    kitty
    broot
    gdb
    tokei
    #### email
    isync
    mu

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
    ffmpegthumbnailer
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
    # packer # not free
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

    # emacs ###################################################################
    ((emacsPackagesFor my-emacs).emacsWithPackages (
      epkgs: with epkgs; [
        mu4e

        # activity-watch-mode
        # ace-link
        # afternoon-theme
        # aggressive-indent
        # ahk-mode
        # alect-themes
        # all-the-icons
        # ample-theme
        # ample-zen-theme
        # anaphora
        # anti-zenburn-theme
        # apropospriate-theme
        # auctex-latexmk
        # auto-compile
        # auto-dictionary
        # auto-highlight-symbol
        # auto-yasnippet
        # badwolf-theme
        # birds-of-paradise-plus-theme
        # blacken
        # bm
        # browse-at-remote
        # bubbleberry-theme
        # bundler
        # busybee-theme
        # centered-cursor-mode
        # # chatgpt
        # cherry-blossom-theme
        # chocolate-theme
        # chruby
        # clean-aindent-mode
        # clues-theme
        # code-cells
        # code-review
        # # codegpt
        # color-theme-sanityinc-solarized
        # color-theme-sanityinc-tomorrow
        # column-enforce-mode
        # command-log-mode
        # company-anaconda
        # company-auctex
        # company-emoji
        # company-lua
        # company-math
        # company-nixos-options
        # company-php
        # company-phpactor
        # company-reftex
        # company-terraform
        # company-web
        # counsel-css
        # counsel-gtags
        # counsel-projectile
        # csv-mode
        # cyberpunk-theme
        # cython-mode
        # dactyl-mode
        # dakrone-theme
        # # dall-e
        # dap-mode
        # darkmine-theme
        # darkokai-theme
        # darktooth-theme
        # define-word
        # devdocs
        # diff-hl
        # diminish
        # dired-quick-sort
        # dired-subtree
        # disable-mouse
        # django-theme
        # doom-themes
        # dotenv-mode
        # dracula-theme
        # drag-stuff
        # drupal-mode
        # dumb-jump
        # # eaf
        # eat
        # ebib
        # ef-themes
        # ein
        # elfeed-goodies
        # elfeed-org
        # elisp-def
        # elisp-demos
        # elisp-slime-nav
        # ellama
        # emmet-mode
        # emoji-cheat-sheet-plus
        # emr
        # esh-help
        # eshell-prompt-extras
        # eshell-z
        # espresso-theme
        # ess
        # ess-R-data-view
        # eval-sexp-fu
        # evil-anzu
        # evil-args
        # evil-cleverparens
        # evil-collection
        # evil-easymotion
        # evil-escape
        # # evil-evilified-state
        # evil-exchange
        # evil-goggles
        # evil-iedit-state
        # evil-indent-plus
        # evil-ledger
        # evil-lion
        # evil-lisp-state
        # evil-lispy
        # evil-matchit
        # evil-mc
        # evil-nerd-commenter
        # evil-numbers
        # evil-org
        # evil-surround
        # evil-tex
        # evil-textobj-line
        # evil-tutor
        # # evil-unimpaired
        # evil-visual-mark-mode
        # evil-visualstar
        # exec-path-from-shell
        # exotica-theme
        # expand-region
        # eyebrowse
        # eziam-themes
        # fancy-battery
        # farmhouse-themes
        # flatland-theme
        # flatui-theme
        # flx-ido
        # flycheck-elsa
        # flycheck-ledger
        # flycheck-package
        # flycheck-pos-tip
        # flyspell-correct-ivy
        # fold-this
        # gandalf-theme
        # geben
        # ggtags
        # gh-md
        # git-link
        # git-messenger
        # git-modes
        # git-timemachine
        # gitignore-templates
        # gnuplot
        # golden-ratio
        # google-translate
        # gotham-theme
        # gptel
        # grandshell-theme
        # graphviz-dot-mode
        # gruber-darker-theme
        # gruvbox-theme
        # hackernews
        # hc-zenburn-theme
        # helm
        # helm-core
        # helm-make
        # hemisu-theme
        # heroku-theme
        # # hide-comnt
        # highlight-indentation
        # highlight-numbers
        # highlight-parentheses
        # hl-todo
        # hledger-mode
        # # holy-mode
        # hungry-delete
        # # hybrid-mode
        # # hyperbole
        # impatient-mode
        # importmagic
        # indent-guide
        # "info+"
        # inkpot-theme
        # inspector
        # ir-black-theme
        # ivy-avy
        # ivy-bibtex
        # ivy-hydra
        # ivy-posframe
        # ivy-purpose
        # ivy-xref
        # ivy-yasnippet
        # jazz-theme
        # jbeans-theme
        # journalctl-mode
        # js-doc
        # js2-refactor
        # json-mode
        # json-navigator
        # json-reformat
        # kaolin-themes
        # keycast
        # light-soap-theme
        # live-py-mode
        # livid-mode
        # load-relative
        # loc-changes
        # lorem-ipsum
        # lsp-ivy
        # lsp-latex
        # lsp-origami
        # lsp-pyright
        # lua-mode
        # lush-theme
        # macrostep
        # madhat2r-theme
        # magit-lfs
        # # majapahit-themes
        # markdown-toc
        # material-theme
        # minimal-theme
        # minitest
        # modus-themes
        # moe-theme
        # molokai-theme
        # monochrome-theme
        # monokai-theme
        # multi-line
        # multi-term
        # multi-vterm
        # mustang-theme
        # mwim
        # nameless
        # naquadah-theme
        # nix-mode
        # noctilux-theme
        # nodejs-repl
        # nov
        # npm-mode
        # obsidian-theme
        # occidental-theme
        # oldlace-theme
        # omtose-phellack-themes
        # open-junk-file
        # org-cliplink
        # org-contrib
        # org-download
        # org-mac-link
        # org-mime
        # org-pomodoro
        # org-present
        # org-projectile
        # org-re-reveal
        # org-ref
        # org-rich-yank
        # org-superstar
        # organic-green-theme
        # orgit-forge
        # osm
        # overseer
        # ox-reveal
        # ox-twbs
        # pandoc-mode
        # paradox
        # password-generator
        # pdf-view-restore
        # phoenix-dark-mono-theme
        # phoenix-dark-pink-theme
        # # php-auto-yasnippets
        # # php-extras
        # phpunit
        # pip-requirements
        # pipenv
        # pippel
        # planet-theme
        # poetry
        # polymode
        # prettier-js
        # professional-theme
        # pug-mode
        # pulsar
        # purple-haze-theme
        # py-isort
        # pydoc
        # pyenv-mode
        # # pylookup
        # pytest
        # python-pytest
        # quickrun
        # railscasts-theme
        # rainbow-delimiters
        # rake
        # ranger
        # rbenv
        # realgud
        # rebecca-theme
        # restart-emacs
        # reverse-theme
        # rjsx-mode
        # robe
        # rspec-mode
        # rubocop
        # rubocopfmt
        # ruby-hash-syntax
        # ruby-refactor
        # ruby-test-mode
        # ruby-tools
        # rvm
        # sass-mode
        # scss-mode
        # seeing-is-believing
        # seti-theme
        # shell-pop
        # slim-mode
        # smeargle
        # smex
        # smyx-theme
        # soft-charcoal-theme
        # soft-morning-theme
        # soft-stone-theme
        # solarized-theme
        # soothe-theme
        # # space-doc
        # spacegray-theme
        # spaceline
        # # spacemacs-purpose-popwin
        # # spacemacs-whitespace-cleanup
        # sphinx-doc
        # spray
        # string-edit-at-point
        # subatomic-theme
        # subatomic256-theme
        # sublime-themes
        # sunny-day-theme
        # symbol-overlay
        # symon
        # systemd
        # tagedit
        # tango-2-theme
        # tango-plus-theme
        # tangotango-theme
        # tao-theme
        # # term-cursor
        # terminal-here
        # test-simple
        # tide
        # timu-macos-theme
        # toc-org
        # toml-mode
        # toxi-theme
        # treemacs-evil
        # treemacs-icons-dired
        # treemacs-magit
        # treemacs-persp
        # treemacs-projectile
        # twilight-anti-bright-theme
        # twilight-bright-theme
        # twilight-theme
        # typescript-mode
        # ujelly-theme
        # underwater-theme
        # undo-fu-session
        # unfill
        # vi-tilde-fringe
        # # vim-powerline
        # vimrc-mode
        # volatile-highlights
        # vundo
        # web-beautify
        # web-mode
        # websocket
        # wfnames
        # wgrep
        # white-sand-theme
        # winum
        # writeroom-mode
        # ws-butler
        # yaml-mode
        # yapfify
        # yasnippet-snippets
        # zen-and-art-theme
        # zenburn-theme
        # # zonokai-emacs
        # zotra
      ]
    ))
    #### emacs dependencies
    glibtool # for vterm in emacs
    delta
  ];
}
