{ pkgs }:
let
  rust-overlay = import (builtins.fetchTarball {
    url = "https://github.com/oxalica/rust-overlay/archive/71a69fd633552550de7cb05cc149e4a99ff6f3b6.tar.gz";
    sha256 = "sha256:17nkwb4nx07g6j3b0nxajh1y304yrxh4fjipmwby28hnrap84rfq";
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
