{ pkgs, lib, ... }:
let
  system = "aarch64-darwin";
  packages = (import ./packages.nix { inherit lib pkgs; });
  common_paths = packages.common_paths { inherit system pkgs; };

  mboxSync = mbox: freq: {
    serviceConfig = {
      Label = "org.tennyson.mbsync-${mbox}";
      UserName = "tennyson";
      StartInterval = freq; # every 1m
      RunAtLoad = true;
      WorkingDirectory = "/Users/tennyson";
      ProgramArguments = [
        "${pkgs.isync}/bin/mbsync"
        "${mbox}"
      ];
      StandardOutPath = "/Users/tennyson/.local/var/log/mbsync-${mbox}.out";
      StandardErrorPath = "/Users/tennyson/.local/var/log/mbsync-${mbox}.error.log";
      EnvironmentVariables = {
        PATH = "${pkgs.sops}/bin:${pkgs.gnupg}/bin:/usr/bin:/bin";
      };
    };
  };
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
  environment.systemPackages =
    with pkgs;
    [
      pam-reattach
      (pkgs.writeScriptBin "firefox-devedition" ''
        #!/usr/bin/env bash
        open -a "Firefox Developer Edition" "$@"
      '')
    ]
    ++ common_paths;

  # [[https://write.rog.gr/writing/using-touchid-with-tmux/#what-files-manages-this][Roger Steve Ruiz | Using TouchID with Tmux]]
  environment.etc."pam.d/sudo_local".text = ''
    # Managed by Nix Darwin
    auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh
    auth       sufficient     pam_tid.so
  '';

  networking.computerName = "onyx";

  services.postgresql = {
    enable = true;
    dataDir = "/Users/tennyson/postgres/data";
    authentication = pkgs.lib.mkOverride 10 ''
      #type database  user    DBuser       auth-method
      local all               all          trust
      host  all       all     127.0.0.1/32 trust
    '';
  };

  launchd.user.agents.firefox-server =
    let
      python = pkgs.python3.withPackages (
        ps: with ps; [
          websockets
          aiohttp
        ]
      );
    in
    {
      serviceConfig = {
        Label = "com.tennysonb.firefox-server";
        ProgramArguments = [
          "${python}/bin/python3"
          "/Users/tennyson/repos/tennysontbardwell/misc-projects/firefox/my-extension/server.py"
        ];
        KeepAlive = true;
        RunAtLoad = true;
        StandardOutPath = "/Users/tennyson/.local/var/log/firefox-server.log";
        StandardErrorPath = "/Users/tennyson/.local/var/log/firefox-server.error.log";
      };
    };

  launchd.daemons.mbsync-fastmail = mboxSync "fastmail" 60;
  launchd.daemons.mbsync-gmail-inbox = mboxSync "gmail-inbox" 60;
  launchd.daemons.mbsync-allmail = mboxSync "gmail-allmail" (3600 * 5);
  launchd.daemons.mbsync-gmail = mboxSync "gmail" (3600 * 1);

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
    taps = [ ];
    brews = [ ];
    casks = [
      "1password"
      "1password-cli"
      "activitywatch"
      "alfred"
      "firefox"
      "firefox@developer-edition"
      "ghostty"
      "hammerspoon"
      "iterm2"
      "readest"
      "tailscale"
      "tailscale-app"
      "thunderbird"
      "tor-browser"
      "ungoogled-chromium"
      "visual-studio-code"
      "vlc"
      "waterfox"
      "zoom"
      # "qflipper"
      # "the-battle-for-wesnoth"
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
