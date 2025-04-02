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


  # nixpkgs' = nixpkgs.applyPatches {
  #   src = nixpkgs;
  #   patches = [ ./r-with-cairo.patch ];
  # };

  # unstablepkgs = unstable.legacyPackages."${system}";
in
{
  nixpkgs-patched = {system}: (import nixpkgs { inherit system; }).applyPatches {
    name = "my-r-with-cario-patch";
    src = nixpkgs;
    patches = [ ./r-with-cairo.patch ];
  };

  pkgs = { nixpkgs, system }: (import (nixpkgs {system = system;}) {
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
}
