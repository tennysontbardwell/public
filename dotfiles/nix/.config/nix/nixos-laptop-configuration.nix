{ config, lib, pkgs, ... }:


let
  system = "x86_64-linux";
  packages = import ./packages.nix { inherit lib pkgs; };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./nixos-laptop-hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/nvme0n1";
  boot.loader.grub.useOSProber = true;

  boot.initrd.luks.devices."luks-086358de-7c9a-42be-a4f8-2fb842748c52".device = "/dev/disk/by-uuid/086358de-7c9a-42be-a4f8-2fb842748c52";
  # Setup keyfile
  boot.initrd.secrets = {
    "/boot/crypto_keyfile.bin" = null;
  };

  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.luks.devices."luks-04c2028b-26ce-4342-8720-7df9e4560aad".keyFile = "/boot/crypto_keyfile.bin";
  boot.initrd.luks.devices."luks-086358de-7c9a-42be-a4f8-2fb842748c52".keyFile = "/boot/crypto_keyfile.bin";
  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.xserver = {
    enable = true;
    dpi = 72;
    desktopManager = {
      xterm.enable = false;
    };
    displayManager = {
      defaultSession = "none+i3";
    };
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
	i3status
	i3lock
      ];
    };
  };

  ## Enable the X11 windowing system.
  #services.xserver.enable = true;

  ## Enable the XFCE Desktop Environment.
  #services.xserver.displayManager.lightdm.enable = true;
  #services.xserver.desktopManager.xfce.enable = true;

  ## Configure keymap in X11
  #services.xserver.xkb = {
  #  layout = "us";
  #  variant = "";
  #};

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  security.pam.services.xscreensaver.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tennyson = {
    isNormalUser = true;
    description = "tennyson";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    #  thunderbird
    ];
  };

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs._1password.enable = true;
  programs._1password-gui.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = map lib.lowPrio ((with pkgs;  [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    gnumake
    fzf
    mpv
    git
    tmux
    zsh
    neovim
    htop
    btop
    texlive.combined.scheme-full
    ranger
    yazi
    python312
    firefox
    yarn
    emacs
    stow
    terminator
    # pulseaudio
    pwvucontrol
    xscreensaver
    ghostty
  #  wget
  ])
    ++ packages.common_paths {
      pkgs = pkgs;
      system = system;
    }
    ++ packages.linux_paths {
      pkgs = pkgs;
      system = system;
    }
  )
  ;

  fonts.packages = with pkgs; [
    noto-fonts
    #nerdfonts.fira-code
    # nerdfonts.droid-sans-mono
    # (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
  ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = pkgs.pinentry-curses;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
