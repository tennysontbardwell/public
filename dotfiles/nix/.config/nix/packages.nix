{
  lib,
  nixpkgs,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  ...
}:
let
  pkgsFor = system:
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
    };
in
{
  pkgs = pkgsFor;

  common_paths = { pkgs, system, ... }:
    [
    ]
      ++ ((import ./tools.nix)
          {pkgs = pkgs;}).paths
      ++ ((import ./python.nix) {
        lib = lib;
        pkgs = pkgs;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      }).paths
      # ++ ((import ./r.nix)
      #     {pkgs = pkgs; unstable-pkgs = pkgs;}).paths
  ;

  linux_paths = { pkgs, system, ... }:
    [
    ]
      ++ ((import ./tools.nix)
          {pkgs = pkgs; unstable-pkgs = pkgs;}).linux_paths
  ;
}
