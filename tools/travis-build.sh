#!/usr/bin/env bash
set -eu
[ -n "$PASSE_TARGET" ]
export NIX_CURL_FLAGS=-sS
if [ ! -e /nix ]; then
	if [ "${CI:-null}" != "true" ]; then
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

set -x
tools/bin/gup -u nix/local.tgz
# first, run a nix-shell to check dependencies
# (verbose; so we only log it if it fails)
if ! nix-shell --run true >log 2>&1; then
	tail -n500 log
	exit 1
fi

# dependencies OK; run a build
set +x
function build {
	nix-build --show-trace
	echo "== Built files:"
	ls -lR result/
}

set -x
# perform appropriate checks on the result
case "$PASSE_TARGET" in
	server)
		build
		./result/bin/passe-server --help >/dev/null
		;;
	client)
		build
		./result/bin/passe --help >/dev/null
		;;
	devel)
		nix-shell --pure --run "gup test"
		;;
	mirage-*)
		# no mirage tests yet
		;;
	*)
		echo "Error:  unknown $PASSE_TARGET"
		exit 1
		;;
esac
