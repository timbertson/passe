#!/usr/bin/env bash
set -eu
[ -n "$PASSE_TARGET" ]
bash <(curl -sS https://gist.githubusercontent.com/timbertson/f643b8ae3a175ba8da7f/raw/travis-nix-bootstrap.sh)

tools/bin/gup -u nix/local.tgz
# first, run a nix-shell to check dependencies
# (verbose; so we only log it if it fails)
if ! nix-shell --show-trace --run true >log 2>&1; then
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
