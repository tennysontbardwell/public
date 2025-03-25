.PHONY: build default

default: build

build:
	nix profile upgrade nix --show-trace && nix run '.#homeConfigurations.tennyson.activationPackage'
