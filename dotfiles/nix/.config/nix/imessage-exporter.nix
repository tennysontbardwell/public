{ pkgs }:
let
  rust-overlay = import (builtins.fetchTarball {
    url = "https://github.com/oxalica/rust-overlay/archive/dec08d5dfeca099b0058f0cc61264b04f33db42c.tar.gz";
    sha256 = "sha256:1xpy5bf7g6rzpya6fp4f7fai5z3w3c7r7bdn9a6nfkvcrhmz9z4c";
  });

  pkgsWithRust = pkgs.extend rust-overlay;
  # pkgsWithRust = import pkgs.path {
  #   overlays = [ rust-overlay ];
  # };

  rustPlatform = pkgsWithRust.makeRustPlatform {
    cargo = pkgsWithRust.rust-bin.stable.latest.default;
    rustc = pkgsWithRust.rust-bin.stable.latest.default;
  };
in
{
  imessage-exporter = rustPlatform.buildRustPackage {
    pname = "imessage-exporter";
    version = "3.1.0";

    src = pkgs.fetchFromGitHub {
      owner = "ReagentX";
      repo = "imessage-exporter";
      rev = "d7046536475119265cb40fdcacaf219051b19afc";
      hash = "sha256-vu73VxSeXp4aQmvafUWcKIVYOWId6QiiuhInJc8cW3U=";
    };

    cargoHash = "sha256-PTl8EX4M2vBa6U/hBE0hhL6kFsAHG/Eu8FuE+x95qvo=";

    nativeBuildInputs = with pkgs; [
      pkg-config
    ];

    buildInputs = with pkgs; [
      openssl
    ];

    # Set timezone for tests
    preCheck = ''
      export TZ=America/Los_Angeles
    '';

    doCheck = false;
  };
}
