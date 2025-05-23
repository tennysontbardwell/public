# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    # 4ed8d70fbe3bc90eb727378fa13abb1563d37b6e is master as of 2025-03-01
    unstable.url = "https://github.com/NixOS/nixpkgs/archive/4ed8d70fbe3bc90eb727378fa13abb1563d37b6e.tar.gz";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    mac-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
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

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      self,
      nixpkgs,
      unstable,
      mac-nixpkgs,
      mac-unstable,
      home-manager,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems,
      disko
  }:
    let
      inherit (nixpkgs) lib;

      packages = (import ./packages.nix {
        lib = lib;
        nixpkgs = nixpkgs;
        unstable = unstable;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      });
      m1-packages = (import ./packages.nix {
        lib = lib;
        nixpkgs = mac-nixpkgs;
        unstable = mac-unstable;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      });

      pkgs = packages.pkgs;
      m1-pkgs = m1-packages.pkgs;

      linux.system = "x86_64-linux";
      linux.pkgs = pkgs linux.system;

      m1.system = "aarch64-darwin";
      m1.pkgs = m1-pkgs m1.system;

      m1.paths = m1-packages.common_paths m1;
      linux.paths = (packages.common_paths linux)
        ++ [
          # sysstat
          # wifi-menu
        ];

      mkPackages = { pkgs, paths, ... }: pkgs.buildEnv {
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

      packages."${m1.system}".default = mkPackages m1;
      devShells."${m1.system}".default = devShells m1;
      # devShells."${m1.system}".uv = {
      #   buildInputs = paths;
      # }

      packages."${linux.system}".default = mkPackages linux;
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

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          disko.nixosModules.disko
          ( { modulesPath, lib, pkgs, ... }:
            import ./pan.nix {
              modulesPath = modulesPath;
              lib = lib;
              pkgs = pkgs;
              packages = packages;
            })
          ./pan-disk-config.nix
          ./pan-hardware-configuration.nix
        ];

        pkgs = pkgs linux.system;
      };
    };
}
