#!/usr/bin/env bash
set -eux
[ -n "$PASSE_TARGET" ]
export NIX_CURL_FLAGS=-sS
if [ ! -e /nix ]; then
	if [ "${CI:None}" != "true" ]; then
		echo "It looks like you're not a CI server, and you don't have a /nix folder. Aborting."
		exit 1
	fi
	echo "=== Installing Nix..."
	# Install Nix
	bash <(curl -sS https://nixos.org/nix/install)
	source $HOME/.nix-profile/etc/profile.d/nix.sh

	# Make sure we can use hydra's binary cache
	sudo mkdir /etc/nix
	sudo tee /etc/nix/nix.conf <<EOF >/dev/null
binary-caches = http://cache.nixos.org http://hydra.nixos.org
trusted-binary-caches = http://hydra.nixos.org
build-max-jobs = 4
EOF
fi

tools/bin/gup -u nix/local.tgz
nix-build --show-trace --argstr target "$PASSE_TARGET" default.nix
tree result
du -hs /nix/store

# perform appropriate checks on the result
case "$PASSE_TARGET" in
	server)
		./result/bin/passe-server --help >/dev/null
		;;
	client)
		./result/bin/passe-server --help >/dev/null
		;;
	devel)
		nix-shell --pure --run "gup test"
		;;
	mirage-*)
		# no mirage tests yet
		;;
	*) echo "Error:  unknown $PASSE_TARGET"

