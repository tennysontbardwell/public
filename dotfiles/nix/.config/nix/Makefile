.PHONY: build default upgrade mac-bounce

default: upgrade

check:
	nix flake check --no-build --show-trace

pan:
	nixos-rebuild switch --flake /root/projects/public/dotfiles/nix/.config/nix'#pan' --show-trace

pan-build:
	nixos-rebuild build --flake /root/projects/public/dotfiles/nix/.config/nix'#pan' --show-trace

pan-upgrade:
	nix flake update nixpkgs
	make pan

onyx:
	nix run 'nix-darwin/nix-darwin-24.11#darwin-rebuild' -- switch --flake '.#onyx'

onyx-build:
	nix run 'nix-darwin/nix-darwin-24.11#darwin-rebuild' -- build --flake '.#onyx'

onyx-upgrade:
	nix flake update mac-nixpkgs nix-darwin mac-emacs-overlay
	make onyx

onyx-bounce:
	# sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.nix-daemon.plist
	sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.activate-system.plist
	sudo launchctl enable system/org.nixos.activate-system.plist

nix-darwin-switch-first-time:
	sudo nix run nix-darwin/nix-darwin-24.11#darwin-rebuild -- switch

nixos-laptop:
	nixos-rebuild --flake '.#nixos-laptop' switch

nixos-laptop-build:
	nixos-rebuild --flake '.#nixos-laptop' build

repl:
	echo run ':lf flake.nix'
	nix repl --experimental-features 'flakes nix-command'

