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

      pkgs = system: import nixpkgs {
        inherit system;
        config.cudaSupport = false;

        overlays = [
          (final: prev: {
            pqiv = prev.callPackage ./overlay/pqiv.nix { };
            sioyek = prev.callPackage ./overlay/sioyek-unstable.nix { };
            gallery-dl = prev.callPackage ./overlay/gallery-dl.nix { };
            rstudioWrapper = prev.callPackage (import stable-inputs.rstudioWrapperFix) {
              packages = [];
              recommendedPackages = with prev.rPackages; [
                boot class cluster codetools foreign KernSmooth lattice MASS
                Matrix mgcv nlme nnet rpart spatial survival
              ];
            };
          })
        ];
      };

      # linux GUI system packages
      # NixOS GUI system
      # inconsolata-nerdfont
      # fonts.packages = [ ... ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts)

      # linux systems
      # sysstat
      # wifi-menu

      unstablepkgs = unstable.legacyPackages."${system}";

      linux = "x86_64-linux";

      m1_system = "aarch64-darwin";

      linux_pkgs = pkgs linux;

      m1_pkgs = pkgs m1_system;

      commonPaths.m1_system = with m1_pkgs;
        [
        ]
          ++ ((import ./tools.nix)
	    {pkgs = m1_pkgs; unstablepkgs = unstable.legacyPackages."${m1_system}";}).paths
          ++ ((import ./python.nix) {pkgs = m1_pkgs;}).paths
          ++ ((import ./r.nix) {pkgs = m1_pkgs;}).paths
      ;

      commonPaths.linux = with linux_pkgs;
        [
        ]
          ++ ((import ./tools.nix)
	    {pkgs = linux_pkgs; unstablepkgs = unstable.legacyPackages."${linux}";}).paths
          ++ ((import ./tools.nix)
	    {pkgs = linux_pkgs; unstablepkgs = unstable.legacyPackages."${linux}";}).linux_paths
          ++ ((import ./python.nix) {pkgs = linux_pkgs;}).paths
          ++ ((import ./r.nix) {pkgs = linux_pkgs;}).paths
      ;
    in
    {
      homeConfigurations =
        (import ./home-manager.nix) {
          nixpkgs = nixpkgs;
          pkgs = m1_pkgs;
          system = m1_system;
          home-manager = home-manager;
        };

      packages."${m1_system}".default = m1_pkgs.buildEnv {
        name = "home-packages";
        paths = commonPaths.m1_system;
      };

      devShells."${m1_system}".default = m1_pkgs.mkShell {
        buildInputs = commonPaths.m1_system;
      };

      nixosConfiguration.linux = nixpkgs.lib.nixosSystem {
        system = linux;
	nix.settings.experimental-features = [ "nix-command" "flake" ];
        environment.systemPackages = commonPaths.linux;
	fonts.packages = with pkgs; [
	  inconslata-nerdfont
          noto-fonts
          noto-fonts-cjk-sans
          noto-fonts-emoji
          liberation_ttf
          fira-code
          fira-code-symbols
          mplus-outline-fonts.githubRelease
          dina-font
          proggyfonts
        ];
      };

      packages."${linux}".linux = linux_pkgs.buildEnv {
        name = "home-packages";
        paths = commonPaths.linux;
      };

      devShells."${linux}".linux = linux_pkgs.mkShell {
        buildInputs = commonPaths.linux;
      };
    };
}
