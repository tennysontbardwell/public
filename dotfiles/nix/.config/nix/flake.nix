# installed on mac via https://zero-to-nix.com/start/install
{
  inputs = {
    # nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs.url = "github:NixOS/nixpkgs";
    # 4ed8d70fbe3bc90eb727378fa13abb1563d37b6e is master as of 2025-03-01
    unstable.url = "https://github.com/NixOS/nixpkgs/archive/4ed8d70fbe3bc90eb727378fa13abb1563d37b6e.tar.gz";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, unstable, home-manager }:
    let
      system = "aarch64-darwin";

      stable-inputs = {
        rstudioWrapperFix = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/NixOS/nixpkgs/63fb588d666ee4b795c6207d5c83c6d6d212a232/pkgs/development/r-modules/wrapper-rstudio.nix";
          sha256 = "sha256:0a6l6awdhzi7iyvql969qim29r9lj0iqz3j7kdn61rg8pgb0jhnc";
        };
      };

      unstablepkgs = unstable.legacyPackages."${system}";

      pkgs = import nixpkgs {
        inherit system;
        config.cudaSupport = false;
        overlays = [
          (final: prev: {
            sioyek = prev.callPackage ./sioyek-unstable.nix { };
            gallery-dl = prev.callPackage ./gallery-dl.nix { };
            rstudioWrapper = prev.callPackage (import stable-inputs.rstudioWrapperFix) {
              packages = [];
              recommendedPackages = with pkgs.rPackages; [
                boot class cluster codetools foreign KernSmooth lattice MASS
                Matrix mgcv nlme nnet rpart spatial survival
              ];
            };
          })
        ];
      };

      # linux GUI system packages
      # dmenu or dmenu-rs

      myRPackages = with pkgs.rPackages;
        [
          dplyr
          forcats
          ggplot2
          htmlwidgets
          purrr
          readr
          stringr
          tibble
          tidyr
          xts
          ggridges
          viridis
          hrbrthemes
          GGally
        ];

      myREnv = pkgs.rWrapper.override{
        packages = myRPackages;
      };

      myRStudio = pkgs.rstudioWrapper.override{
        packages = myRPackages;
      };

      myPythonEnv = pkgs.python3.withPackages (ps: with ps; with pkgs.python312Packages;
        [
          ### misc/basic
          requests
          lxml
          beautifulsoup4
          pypdf
          tqdm
          pip
          aiohttp
          scrapy

          ### Data
          matplotlib
          numpy
          pandas
          polars
          fastparquet
          parquet
          jupyter
          jupyterlab
          plotnine
          scikit-learn
          scikit-misc
          spyder-kernels

          ### AI
          openai
          parquet
          torch
          transformers
          torchvision
          torchaudio
          huggingface-hub
          # vllm

          ## misc from vllm debugging
          aioprometheus
          fastapi
          lm-format-enforcer
          outlines
          psutil
          py-cpuinfo
          pyarrow
          pydantic
          pyzmq
          ray
          sentencepiece
          tiktoken
          uvicorn
          xformers
          # prometheus-fastapi-instrumentator

          ## not FOSS / not mac
          # accelerate
          # bitsandbytes
      ]);

      commonPaths = with pkgs;
        [
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

          #### compression & data
          zstd
          xz
          brotli
          parquet-tools
          hdf5

          #### software stacks
          # java
          maven
          # web
          nodejs
          yarn
          php
          # python
          myPythonEnv
          uv
          jetbrains.pycharm-community
          pipx
          # R
          myREnv
          myRStudio

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
          # inotify-tools  = fs watch + cmd runner

        ];
      commonEnv = pkgs.buildEnv {
        name = "home-packages";
        paths = commonPaths;
      };
    in
    {
      homeConfigurations.tennyson =
        let
          pkgs = import nixpkgs { inherit system; };
        in
        home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          {
            home = {
              username = "tennyson";
              homeDirectory = "/Users/tennyson/";
              stateVersion = "25.05";
              packages = with pkgs; [
                # browser
                firefox
                librewolf

                # academic
                zotero

                # media
                vlc-bin
                mpv
                audacity

                # sys util
                tailscale
                sioyek
                openconnect
                utm

                # util
                keepassxc
                unstablepkgs.emacs30
                source-code-pro # not sure if this is working, maybe return to brew --cask for it

                # not on mac
                # mullvad-vpn
                # labplot
                # mumble
                # obs
                # activitywatch
                # kiwix

                # broken
                # yacreader

                # not in nix (maybe not free?)
                # alfred # cask
                # audio-hijack # cask
                # handbreak # cask?

                # not used at the moment
                # textual

              ];
            };
            programs.librewolf = {
              enable = true;
              # Enable WebGL, cookies and history
              settings = {
                "webgl.disabled" = false;
                "privacy.resistFingerprinting" = false;
                "privacy.clearOnShutdown.history" = false;
                "privacy.clearOnShutdown.cookies" = false;
                "network.cookie.lifetimePolicy" = 0;
              };
            };
          }
        ];
      };

      packages.${system}.default = pkgs.buildEnv {
        name = "home-packages";
        paths = commonPaths
        ;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = commonPaths
        ;
      };
    };
}
