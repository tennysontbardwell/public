# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/";

    nixos-laptop-nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "mac-nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mac-emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "mac-nixpkgs";
      inputs.nixpkgs-stable.follows = "mac-nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixos-laptop-nixpkgs,
      mac-nixpkgs,
      nix-darwin,
      disko,
      mac-emacs-overlay,
    }:
    {
      nix-darwin.overlays = [
        mac-emacs-overlay.overlay
      ];

      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./onyx-config.nix
        ];
      };

      nixosConfigurations.nixos-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          ./nixos-laptop-configuration.nix
        ];

        # pkgs = pkgs linux.system;
      };

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          disko.nixosModules.disko
          ./pan.nix
          ./pan-disk-config.nix
          ./pan-hardware-configuration.nix
        ];

        # pkgs = pkgs linux.system;
      };
    };
}
