{ nixpkgs, pkgs, system, home-manager, ... }:
{
  tennyson =
    let
      pkgs = import nixpkgs { inherit system; };
    in
    home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [{
      home = {
        username = "tennyson";
        homeDirectory = "/Users/tennyson/";
        stateVersion = "25.05";
        packages = with pkgs; [
          # browser
          # firefox
          # librewolf

          # academic
          zotero

          # media
          vlc-bin
          mpv
          audacity

          # sys util
          tailscale
          sioyek
          openconnect
          utm

          # util
          keepassxc
          source-code-pro # not sure if this is working, maybe return to brew --cask for it

          # not on mac
          # mullvad-vpn
          # labplot
          # mumble
          # obs
          # activitywatch
          # kiwix

          # broken
          # yacreader
          # qview

          # not in nix (maybe not free?)
          # alfred # cask
          # audio-hijack # cask
          # handbreak # cask?

          # not used at the moment
          # textual

        ];
      };
    }];
  };
}
