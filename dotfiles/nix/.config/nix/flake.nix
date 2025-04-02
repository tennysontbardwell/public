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

    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      self,
      nixpkgs,
      unstable,
      home-manager,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems
  }:
    let
      inherit (nixpkgs) lib;

      # nixpkgs' = nixpkgs.applyPatches {
      #   src = nixpkgs;
      #   patches = [ ./r-with-cairo.patch ];
      # };

      stable-inputs = {
        rstudioWrapperFix = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/NixOS/nixpkgs/63fb588d666ee4b795c6207d5c83c6d6d212a232/pkgs/development/r-modules/wrapper-rstudio.nix";
          sha256 = "sha256:0a6l6awdhzi7iyvql969qim29r9lj0iqz3j7kdn61rg8pgb0jhnc";
        };
      };


      nixpkgs-patched = {system}: (import nixpkgs { inherit system; }).applyPatches {
        name = "my-r-with-cario-patch";
        src = nixpkgs;
        patches = [ ./r-with-cairo.patch ];
      };
      # pkgs = import nixpkgs-patched { inherit system; };


      pkgs = system: (import (nixpkgs-patched {system = system;}) {
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
            # python3Packages.build = prev.callPackage (import stable-inputs.rstudioWrapperFix) {
          })
        ];
      });

      # unstablepkgs = unstable.legacyPackages."${system}";

      linux.system = "x86_64-linux";
      linux.pkgs = pkgs linux.system;

      m1.system = "aarch64-darwin";
      m1.pkgs = pkgs m1.system;

      common_paths = { pkgs, system, ... }:
        [
        ]
          ++ ((import ./tools.nix)
	           {pkgs = pkgs; unstablepkgs = unstable.legacyPackages."${system}";}).paths
          ++ ((import ./python.nix) {
            lib = lib;
            pkgs = pkgs;
            pyproject-nix = pyproject-nix;
            uv2nix = uv2nix;
            pyproject-build-systems = pyproject-build-systems;
          }).paths
          ++ ((import ./r.nix) {pkgs = pkgs;}).paths
      ;

      m1.paths = common_paths m1;
      linux.paths = common_paths linux
        ++ [
          # sysstat
          # wifi-menu
        ];

      packages = { pkgs, paths, ... }: pkgs.buildEnv {
        name = "home-packages";
        paths = paths;
      };

      devShells = { pkgs, paths, ... }: pkgs.buildEnv {
        buildInputs = paths;
      };
    in
    {
      homeConfigurations =
        (import ./home-manager.nix) {
          nixpkgs = nixpkgs;
          pkgs = m1.pkgs;
          system = m1.system;
          home-manager = home-manager;
        };

      packages."${m1.system}".default = packages m1;
      devShells."${m1.system}".default = devShells m1;
      # devShells."${m1.system}".uv = {
      #   buildInputs = paths;
      # }

      packages."${linux.system}".default = packages linux;
      devShells."${linux.system}".default = devShells linux;


      nixosConfiguration.linux = nixpkgs.lib.nixosSystem {
        system = linux.system;
	      nix.settings.experimental-features = [ "nix-command" "flake" ];
        environment.systemPackages = linux.paths;
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

      nixosConfiguration.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./pan.nix
          ./pan-disk-config.nix
          ./pan-hardware-configuration.nix
        ];
      };
    };
}
