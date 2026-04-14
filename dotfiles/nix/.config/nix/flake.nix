# installed on mac via https://zero-to-nix.com/start/install
{
  description = "tennyson-nix";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/";

    mac-nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "mac-nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mac-emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "mac-nixpkgs";
      inputs.nixpkgs-stable.follows = "mac-nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      mac-nixpkgs,
      nix-darwin,
      disko,
      mac-emacs-overlay,
    }:
    {
      darwinConfigurations.onyx = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          {
            nixpkgs.overlays = [
              mac-emacs-overlay.overlay

              (final: prev: {
                ffmpeg = prev.ffmpeg.overrideAttrs (old: {
                  postFixup = (old.postFixup or "") + ''
                    for f in "$out"/lib/*.dylib; do
                      if [ -f "$f" ]; then
                        /usr/bin/codesign --force --sign - "$f"
                      fi
                    done
                    if [ -d "$out/bin" ]; then
                      for f in "$out"/bin/*; do
                        if [ -f "$f" ]; then
                          /usr/bin/codesign --force --sign - "$f"
                        fi
                      done
                    fi
                  '';
                });
              })
            ];
          }
          ./hosts/onyx-config.nix
        ];
      };

      nixosConfigurations.nixos-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./hosts/nixos-laptop-configuration.nix
        ];
      };

      nixosConfigurations.pan = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          disko.nixosModules.disko
          ./hosts/pan.nix
          ./hosts/pan-disk-config.nix
          ./hosts/pan-hardware-configuration.nix
        ];
      };

      packages.aarch64-linux.hello-docker = import ./docker/hello.nix {
        inherit nixpkgs;
        system = "aarch64-linux";
      };

      packages.aarch64-darwin.hello-docker = self.packages.aarch64-linux.hello-docker;
    };
}
