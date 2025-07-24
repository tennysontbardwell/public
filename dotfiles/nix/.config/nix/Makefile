.PHONY: build default upgrade mac-bounce

default: upgrade

pan:
	nixos-rebuild switch --flake /root/projects/public/dotfiles/nix/.config/nix'#pan' --show-trace

pan-build:
	nixos-rebuild build --flake /root/projects/public/dotfiles/nix/.config/nix'#pan' --show-trace

onyx:
	nix run 'nix-darwin/nix-darwin-24.11#darwin-rebuild' -- switch --flake '.#onyx'

upgrade:
	nix profile upgrade dotfiles/nix/.config/nix --show-trace && nix run '.#homeConfigurations.tennyson.activationPackage'

nix-darwin-switch-first-time:
	sudo nix run nix-darwin/nix-darwin-24.11#darwin-rebuild -- switch

update-mac-inputs:
	nix flake update mac-nixpkgs mac-unstable home-manager

build:
	nix build . --show-trace --keep-failed

repl:
	echo run ':lf flake.nix'
	nix repl --experimental-features 'flakes nix-command'

mac-bounce:
	sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.nix-daemon.plist
