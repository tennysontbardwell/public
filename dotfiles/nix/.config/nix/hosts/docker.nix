{ nixpkgs, system }:

let
  pkgs = import nixpkgs { inherit system; };
in
pkgs.dockerTools.buildImage {
  name = "hello-docker";
  tag = "latest";
  contents = [ pkgs.hello ];
  config.Cmd = [ "${pkgs.hello}/bin/hello" ];
}
