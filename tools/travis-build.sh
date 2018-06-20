#!/usr/bin/env bash
set -eu
[ -n "$PASSE_TARGET" ]

# first, run a nix-shell to check dependencies
# (verbose; so we only log it if it fails)
if ! nix-shell --show-trace --run true >log 2>&1; then
	tail -n500 log
	exit 1
fi

# dependencies OK; run a build
set +x
function build {
	NIX_PIN="$(nix-build --no-out-link '<nixpkgs>' -A 'nix-pin')/bin/nix-pin"
	"$NIX_PIN" status | grep -q passe || "$NIX_PIN" create passe .
	"$NIX_PIN" build
	"$NIX_PIN" build --show-trace
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
