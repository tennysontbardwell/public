# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
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
      mac-nixpkgs,
      nix-darwin,
      disko,
      mac-emacs-overlay,
    }:
    let
      dockerSystem = "aarch64-linux";
      pkgsDocker = import nixpkgs { system = dockerSystem; };
    in
    {
      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          # { nixpkgs.overlays = [ mac-emacs-overlay.overlay ]; }
          ./hosts/onyx-config.nix
        ];
      };

      nixosConfigurations.nixos-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/nixos-laptop-configuration.nix
        ];
      };

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          disko.nixosModules.disko
          ./hosts/pan.nix
          ./hosts/pan-disk-config.nix
          ./hosts/pan-hardware-configuration.nix
        ];
      };

      packages.${dockerSystem}.hello-docker = pkgsDocker.dockerTools.buildImage {
        name = "hello-docker";
        config = {
          Cmd = [ "${pkgsDocker.hello}/bin/hello" ];
        };
      };

      packages.aarch64-darwin.hello-docker = self.packages.aarch64-linux.hello-docker;
    };
}
