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
          firefox
          librewolf

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
          emacs30
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

          # not in nix (maybe not free?)
          # alfred # cask
          # audio-hijack # cask
          # handbreak # cask?

          # not used at the moment
          # textual

        ];
      };
      programs.librewolf = {
        enable = true;
        # Enable WebGL, cookies and history
        settings = {
          "webgl.disabled" = false;
          "privacy.resistFingerprinting" = false;
          "privacy.clearOnShutdown.history" = false;
          "privacy.clearOnShutdown.cookies" = false;
          "network.cookie.lifetimePolicy" = 0;
        };
      };
    }];
  };
}
