{ pkgs, lib, mac-nixpkgs, pyproject-nix, uv2nix, pyproject-build-systems, ... }:
let
  system = "aarch64-darwin";
  # patch is currently disabled on onyx
  # pkgs = (import ./patch.nix) { nixpkgs = mac-nixpkgs; } system;
  packages = (import ./packages.nix {
    inherit lib pkgs pyproject-nix uv2nix pyproject-build-systems;
  });
  paths = packages.common_paths { inherit system pkgs; };
in
{
  # see https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-homebrew.masApps
  nix = {
    enable = false;
    settings.experimental-features = "nix-command flakes";
  };
  nixpkgs.hostPlatform = system;
  users.users.tennyson = {
      name = "tennyson";
      home = "/Users/tennyson";
  };
  programs.zsh.enable = true;
  environment.systemPackages = paths ++ [pkgs.pam-reattach];

  # [[https://write.rog.gr/writing/using-touchid-with-tmux/#what-files-manages-this][Roger Steve Ruiz | Using TouchID with Tmux]]
  environment.etc."pam.d/sudo_local".text = ''
    # Managed by Nix Darwin
    auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh
    auth       sufficient     pam_tid.so
  '';


  networking.computerName = "onyx";
  security.pam.services.sudo_local.touchIdAuth = true;
  system = {
    primaryUser = "tennyson";
    activationScripts.postActivation.text = ''
        echo "Running my custom activation script..."
        cd /Users/tennyson/repos/tennysontbardwell/public/dotfiles
        # sudo -u tennyson stow -t /Users/tennyson sioyek vim zsh tmux ranger hammerspoon aws bash visidata
        cd /Users/tennyson/repos/tennysontbardwell/dotfiles
        # sudo -u tennyson stow -t /Users/tennyson aspell borg emacs git misc pass pgp scripts secrets tennyson.py zsh
    '';
    # TODO
    # configurationRevision = self.rev or self.dirtyRev or null;
    defaults = {
      screensaver.askForPasswordDelay = 5;
      loginwindow.GuestEnabled = true;
      NSGlobalDomain = {
        "com.apple.trackpad.scaling" = 1.0;
        AppleShowAllExtensions = true;
        InitialKeyRepeat = 15;
        NSAutomaticSpellingCorrectionEnabled = false;
      };
      controlcenter = {
        AirDrop = false;
        BatteryShowPercentage = true;
        Bluetooth = true;
      };
      dock.autohide-delay = 0.2;
      dock.wvous-br-corner = 4;
      menuExtraClock = {
        ShowSeconds = true;
        Show24Hour = true;
      };
    };
    keyboard.enableKeyMapping = true;
    keyboard.remapCapsLockToControl = true;
    stateVersion = 4;
  };
  homebrew = {
      enable = true;
      # onActivation.cleanup = "uninstall";

      masApps = {
        Xcode = 497799835;
        # "1Password 7 - Password Manager" = 1333542190;
      };
      taps = [];
      brews = [ ];
      casks = [
        "1password-cli"
        "1password"
        "activitywatch"
        "alfred"
        "firefox"
        "firefox@developer-edition"
        "hammerspoon"
        "readest"
        "tailscale"
        "thunderbird"
        "tor-browser"
        "visual-studio-code"
      ];
  };
  fonts.packages = with pkgs; [
    nerd-fonts.noto
    nerd-fonts."m+"
    nerd-fonts.hack
    nerd-fonts.tinos
    nerd-fonts.monoid
  ];
}
