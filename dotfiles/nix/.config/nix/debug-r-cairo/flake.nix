# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix-debug";

  inputs = {
    # nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = {
      self,
      nixpkgs,
  }:
    let
      inherit (nixpkgs) lib;
      system = "aarch64-darwin";

      nixpkgs-patched = (import nixpkgs { inherit system; }).applyPatches {
        name = "my-r-with-cario-patch";
        src = nixpkgs;
        patches = [ ./r-with-cairo.patch ];
      };

      pkgs = import (nixpkgs-patched) {
        inherit system;
      };
    in
    {
      packages."${system}".default = pkgs.buildEnv {
        name = "home-packages";
        paths = [ pkgs.R ];
      };
    };
}
