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
	# nix flake update mac-nixpkgs nix-darwin mac-emacs-overlay
	nix flake update mac-nixpkgs nix-darwin
	make onyx

onyx-bounce:
	# sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.nix-daemon.plist
	sudo launchctl bootstrap system /Library/LaunchDaemons/org.nixos.activate-system.plist
	sudo launchctl enable system/org.nixos.activate-system.plist

linux-vm-remote-builder-init:
	limactl stop com.tennysontbardwell.nixbuilder || :
	limactl delete com.tennysontbardwell.nixbuilder || :
	limactl create --tty=false --name com.tennysontbardwell.nixbuilder nixbuilder.yaml

linux-vm-remote-builder-build:
	limactl start com.tennysontbardwell.nixbuilder
	limactl shell com.tennysontbardwell.nixbuilder sh -c 'cd /mnt/src && nix build ".#hello-docker" --out-link /mnt/out/result && cp /mnt/out/result /mnt/out/docker-hello-image.tar.gz && unlink /mnt/out/result'
	limactl stop com.tennysontbardwell.nixbuilder
	podman load -i ~/excluded-backup/nix-artifacts/docker-hello-image.tar.gz

nix-darwin-switch-first-time:
	sudo nix run nix-darwin/nix-darwin-24.11#darwin-rebuild -- switch

nixos-laptop:
	nixos-rebuild --flake '.#nixos-laptop' switch

nixos-laptop-build:
	nixos-rebuild --flake '.#nixos-laptop' build

repl:
	echo run ':lf flake.nix'
	nix repl --experimental-features 'flakes nix-command'

