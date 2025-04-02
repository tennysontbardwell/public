{
  modulesPath,
  lib,
  pkgs,
  packages,
  ...
}:
let
  system = "x86_64-linux";
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./pan-disk-config.nix
  ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  boot = {
    supportedFilesystems = [ "zfs" ];
    kernelParams = [ "ip=95.217.85.40" ];
    initrd = {
      availableKernelModules = [ "e1000e" ];
      luks.devices.crypted.device = "/dev/disk/by-partuuid/046f86a3-f5ff-45d3-bdb7-df7fb8681e66";
      # systemd.users.root.shell = "/bin/cryptsetup-askpass";
      network = {
        enable = true;
        ssh = {
          enable = true;
          port = 22;
          authorizedKeys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAaLrS6oh5HgcBveCV6xVQb/oco4lJOzpG3erGkaSW7r tennyson@artemis" ];
          hostKeys = [ "/tmp/secrets/id_ed25519" ];
        };
      };
    };
    loader.grub = {
      # no need to set devices, disko will add all devices that have a EF02 partition to the list already
      # devices = [ ];
      zfsSupport = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      enableCryptodisk = true;
    };
  };
  # fileSystems."/mount" = {
  #   device = "zmain/test";
  #   fsType = "zfs";
  # };

  networking.hostId = "1c816d71";
  networking.hostName = "pan";

  services.openssh.enable = true;
  services.tailscale.enable = true;
  security.acme = {
    acceptTerms = true;
    defaults.email = "dev-null@tennysontbardwell.com";
    certs."pan.tennysontbardwell.com" = {
      dnsProvider = "route53";
      environmentFile = "/root/secrets/aws-keys.env";
      # set to make nginx happy
      webroot = null;
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    # other Nginx options
    virtualHosts."pan.tennysontbardwell.com" =  {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8096";
        proxyWebsockets = true; # needed if you need to use WebSocket
        # extraConfig =
          # required when the target is also TLS server with multiple hosts
          # "proxy_ssl_server_name on;" +
          # required when the server wants to use HTTP Authentication
          # "proxy_pass_header Authorization;"
          # ;
      };
    };
  };

  environment.systemPackages = (with pkgs; map lib.lowPrio [
    curl
    gitMinimal
    vim
    mosh
    tmux
    cryptsetup
    zfs
    zstd

    ripgrep
    jq
    wget
    tmux
    pandoc
    fzf
    socat
    rsync
    neovim
    vim
    ncdu
    htop
    git-lfs
    pv
    texlive.combined.scheme-full
    pipx
    parallel
    git
    findutils
    curl
    ffmpeg
    imagemagick
    graphviz
    nixos-anywhere

    jellyfin
    tailscale
    python3
    git-annex
    pass
    coreutils-full
    #gnupg
    pinentry-all
    stow
    zsh
    ranger
  ])
    ++ packages.common_paths {
      pkgs = pkgs { system = system; };
      system = system;
    }
  ;

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # programs.gnupg.agent.pinentryPackage.enabled = true;
  services.pcscd.enable = true;
  programs.gnupg.agent = {
   enable = true;
   # pinentryFlavor = "curses";
   enableSSHSupport = true;
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAaLrS6oh5HgcBveCV6xVQb/oco4lJOzpG3erGkaSW7r tennyson@artemis"
  ];

  system.stateVersion = "24.05";

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 80 443 ];
    allowedUDPPorts = [ 22 ];
    # allowedUDPPortRanges = [
      # { from = 4000; to = 4007; }
      # { from = 8000; to = 8010; }
    # ];
  };
}
