{
  lib,
  nixpkgs,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  ...
}:
{
  pkgs = ./patch nixpkgs;

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
