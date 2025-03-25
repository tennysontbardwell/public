# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

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

        emacs30 = unstablepkgs.emacs30 { };

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
      # dolphin
      # gnome-screenshot
      # i3
      # rxvt-unicode
      # synapse
      # thunar
      # xorg

      # NixOS GUI system
      # inconsolata-nerdfont
      # fonts.packages = [ ... ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts)

      # linux systems
      # sysstat
      # wifi-menu

      commonPaths = with pkgs;
        [
        ]
          ++ ((import ./tools.nix) {pkgs = pkgs; unstablepkgs = unstablepkgs;}).paths
          ++ ((import ./python.nix) {pkgs = pkgs;}).paths
          ++ ((import ./r.nix) {pkgs = pkgs;}).paths
      ;

      commonEnv = pkgs.buildEnv {
        name = "home-packages";
        paths = commonPaths;
      };
    in
    {
      homeConfigurations =
        (import ./home-manager.nix) {
          nixpkgs = nixpkgs;
          pkgs = pkgs;
          system = system;
          home-manager = home-manager;
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
