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
    in
    {
      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./onyx-config.nix
          ./r.nix
        ];
      };

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        inherit (linux) "x86_64-linux";

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
          ./r.nix
        ];

        pkgs = pkgs linux.system;
      };
    };
}
