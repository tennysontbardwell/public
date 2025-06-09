{
  lib,
  nixpkgs,
  unstable,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  ...
}:
let
  stable-inputs = {
    rstudioWrapperFix = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixpkgs/63fb588d666ee4b795c6207d5c83c6d6d212a232/pkgs/development/r-modules/wrapper-rstudio.nix";
      sha256 = "sha256:0a6l6awdhzi7iyvql969qim29r9lj0iqz3j7kdn61rg8pgb0jhnc";
    };
  };

  # pkgs-patched = { system }: (import nixpkgs { inherit system; }).applyPatches {
  #   name = "my-nixpkgs-patched";
  #   src = nixpkgs;
  #   # patches = [ ./r-with-cairo.patch ];
  #   patches = [ ];
  # };
  pkgs-patched = { system }: (import unstable { inherit system; }).applyPatches {
    name = "my-unstable-patched";
    src = unstable;
    # patches = [ ./r-with-cairo.patch ];
    patches = [ ];
  };

  # nixpkgs' = nixpkgs.applyPatches {
  #   src = nixpkgs;
  #   patches = [ ./r-with-cairo.patch ];
  # };

  # unstablepkgs = unstable.legacyPackages."${system}";
in
{
  pkgs = system: (import (pkgs-patched {system = system;}) {
    inherit system;
    config.cudaSupport = false;

    overlays = [
      (final: prev: {
        sioyek = prev.callPackage ./overlay/sioyek-unstable.nix { };
      })
    ];
  });

  common_paths = { pkgs, system, ... }:
    [
    ]
      ++ ((import ./tools.nix)
          {pkgs = pkgs; unstable-pkgs = pkgs;}).paths
      ++ ((import ./python.nix) {
        lib = lib;
        pkgs = pkgs;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      }).paths
      ++ ((import ./r.nix)
          {pkgs = pkgs; unstable-pkgs = pkgs;}).paths
  ;

  linux_paths = { pkgs, system, ... }:
    [
    ]
      ++ ((import ./tools.nix)
          {pkgs = pkgs; unstable-pkgs = unstable.legacyPackages."${system}";}).linux_paths
  ;
}
