.PHONY: build default upgrade mac-bounce

default: upgrade

upgrade:
	nix profile upgrade dotfiles/nix/.config/nix --show-trace && nix run '.#homeConfigurations.tennyson.activationPackage'

build:
	nix build . --show-trace --keep-failed

repl:
	echo run ':lf flake.nix'
	nix repl --experimental-features 'flakes nix-command'

mac-bounce:
	sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.nix-daemon.plist
