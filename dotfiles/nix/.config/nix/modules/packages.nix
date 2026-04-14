{ lib, pkgs, ... }:
{
  common_paths = { pkgs, system, ... }:
    let
      tools  = import ./tools.nix { pkgs = pkgs; };
      r      = import ./r.nix     { pkgs = pkgs; };
      python = import ./python/python.nix
        { inherit lib pkgs; };
      emacs  = import ./emacs.nix { pkgs = pkgs; };
    in
    []
      ++ tools.paths
      ++ python.paths
      ++ r.paths
      ++ emacs.paths
  ;

  linux_paths = { pkgs, system, ... }:
    let
      tools  = import ./tools.nix { pkgs = pkgs; };
    in
    []
      ++ tools.linux_paths
  ;
}
