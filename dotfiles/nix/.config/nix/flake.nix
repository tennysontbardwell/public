# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "mac-nixpkgs";


    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
      self,
      nixpkgs,
      mac-nixpkgs,
      home-manager,
      nix-darwin,
      pyproject-nix,
      uv2nix,
      pyproject-build-systems,
      disko
  }:
    let
      inherit (nixpkgs) lib;

      packages = (import ./packages.nix {
        lib = lib;
        nixpkgs = nixpkgs;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      });
      m1-packages = (import ./packages.nix {
        lib = lib;
        nixpkgs = mac-nixpkgs;
        pyproject-nix = pyproject-nix;
        uv2nix = uv2nix;
        pyproject-build-systems = pyproject-build-systems;
      });

      pkgs = packages.pkgs;
      m1-pkgs = m1-packages.pkgs;

      linux.system = "x86_64-linux";
      linux.pkgs = pkgs linux.system;

      m1.system = "aarch64-darwin";
      m1.pkgs = m1-pkgs m1.system;

      m1.paths = m1-packages.common_paths m1;
      linux.paths = (packages.common_paths linux)
        ++ [
          # sysstat
          # wifi-menu
        ];

      mkPackages = { pkgs, paths, ... }: pkgs.buildEnv {
        name = "home-packages";
        paths = paths;
      };

      devShells = { pkgs, paths, ... }: pkgs.buildEnv {
        buildInputs = paths;
      };

      onyx_config = { pkgs, ... }:
      let
        m1.paths = m1-packages.common_paths {
          pkgs = pkgs;
          system = "aarch64-darwin";
        };
      in
      {
        # see https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-homebrew.masApps
        nix = {
          enable = false;
          settings.experimental-features = "nix-command flakes";
        };
        nixpkgs.hostPlatform = "aarch64-darwin";
        users.users.tennyson = {
            name = "tennyson";
            home = "/Users/tennyson";
        };
        programs.zsh.enable = true;
        # environment.systemPackages = with pkgs; [ libfaketime emacs mas neovim R stow iterm2 fzf tmux nodejs yarn ranger ripgrep ];
        # environment.systemPackages = m1-packages.common_paths {
        #   pkgs = m1.pkgs;
        #   system = m1.system;
        # };
        environment.systemPackages = m1.paths ++ [pkgs.pam-reattach];
        # [[https://write.rog.gr/writing/using-touchid-with-tmux/#what-files-manages-this][Roger Steve Ruiz | Using TouchID with Tmux]]
        environment.etc."pam.d/sudo_local".text = ''
          # Managed by Nix Darwin
          auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh
          auth       sufficient     pam_tid.so
        '';


        # environment.systemPackages = m1-packages.pkgs;
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
          configurationRevision = self.rev or self.dirtyRev or null;
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
              "1Password 7 - Password Manager" = 1333542190;
            };
            taps = [];
            brews = [ ];
            casks = [
              "firefox"
              "tailscale"
              "thunderbird"
              "activitywatch"
              "1password-cli"
              "alfred"
              "hammerspoon"
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
      };
    in
    {
      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          onyx_config
        ];
      };

      # packages."${m1.system}".onyx = self.darwinConfigurations.onyx.system;

      # homeConfigurations =
      #   (import ./home-manager.nix) {
      #     nixpkgs = nixpkgs;
      #     pkgs = m1.pkgs;
      #     system = m1.system;
      #     home-manager = home-manager;
      #   };

      # packages."${m1.system}".default = mkPackages m1;
      # devShells."${m1.system}".default = devShells m1;
      # devShells."${m1.system}".uv = {
      #   buildInputs = paths;
      # }

      packages."${linux.system}".default = mkPackages linux;
      devShells."${linux.system}".default = devShells linux;


      nixosConfiguration.linux = nixpkgs.lib.nixosSystem {
        system = linux.system;
	      nix.settings.experimental-features = [ "nix-command" "flake" ];
        environment.systemPackages = linux.paths;
	      fonts.packages = with pkgs; [
          # nerd-fonts.m+
          inconslata-nerdfont
          noto-fonts
          noto-fonts-cjk-sans
          noto-fonts-emoji
          liberation_ttf
          fira-code
          fira-code-symbols
          mplus-outline-fonts.githubRelease
          dina-font
          proggyfonts
        ];
      };

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";

        modules = [
          disko.nixosModules.disko
          ( { modulesPath, lib, pkgs, ... }:
            import ./pan.nix {
              modulesPath = modulesPath;
              lib = lib;
              pkgs = pkgs;
              packages = packages;
            })
          ./pan-disk-config.nix
          ./pan-hardware-configuration.nix
        ];

        pkgs = pkgs linux.system;
      };
    };
}
