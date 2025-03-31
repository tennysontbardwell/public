.PHONY: build default upgrade

default: upgrade

upgrade:
	nix profile upgrade dotfiles/nix/.config/nix --show-trace && nix run '.#homeConfigurations.tennyson.activationPackage'

build:
	nix build . --show-trace
