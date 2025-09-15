{
  lib,
  pkgs,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  ...
}:
{
  common_paths = { pkgs, system, ... }:
    let
      tools  = import ./tools.nix { pkgs = pkgs; };
      r      = import ./r.nix     { pkgs = pkgs; };
      python = import ./python.nix
        { inherit lib pkgs pyproject-nix uv2nix pyproject-build-systems; };
    in
    []
      ++ tools.paths
      ++ python.paths
      ++ r.paths
  ;

  linux_paths = { pkgs, system, ... }:
    []
      ++ ((import ./tools.nix)
          {pkgs = pkgs; unstable-pkgs = pkgs;}).linux_paths
  ;
}
