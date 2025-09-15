{ nixpkgs, ... }:
system:
  import nixpkgs {
    inherit system;

    config = {
      cudaSupport = false;
    };

    overlays = [
      (final: prev: {
        sioyek = prev.callPackage ./overlay/sioyek-unstable.nix { };
      })
    ];
  }
