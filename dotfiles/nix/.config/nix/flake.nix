# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "mac-nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "mac-nixpkgs";


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
      mac-nixpkgs,
      home-manager,
      nix-darwin,
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
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      });

      pkgs = packages.pkgs;

      linux.system = "x86_64-linux";
      linux.pkgs = pkgs linux.system;

      m1.system = "aarch64-darwin";

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
      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./onyx-config.nix
        ];
      };

      # packages."${m1.system}".onyx = self.darwinConfigurations.onyx.system;

      # homeConfigurations =
      #   (import ./home-manager.nix) {
      #     nixpkgs = nixpkgs;
      #     pkgs = m1.pkgs;
      #     system = m1.system;
      #     home-manager = home-manager;
      #   };

      # packages."${m1.system}".default = mkPackages m1;
      # devShells."${m1.system}".default = devShells m1;
      # devShells."${m1.system}".uv = {
      #   buildInputs = paths;
      # }

      packages."${linux.system}".default = mkPackages linux;
      devShells."${linux.system}".default = devShells linux;

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
