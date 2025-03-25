.PHONY: build default

default: build

build:
	nix profile upgrade dotfiles/nix/.config/nix --show-trace && nix run '.#homeConfigurations.tennyson.activationPackage'
