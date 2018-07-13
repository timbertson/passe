#!/usr/bin/env bash
set -eu
[ -n "$PASSE_TARGET" ]

if [ "$TRAVIS" = true -a -n "$CACHIX_AUTH" ]; then
	echo "Setting up cachix..."
	nix-env -if https://github.com/cachix/cachix/tarball/master \
		--substituters https://cachix.cachix.org \
		--trusted-public-keys 'cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM='
	cachix authtoken "$CACHIX_AUTH"
	cachix use timbertson
	cachix push timbertson -w &
	cachix_pid=$!
	function cachix_wait() {
		status=$?
		set +x
		kill "$cachix_pid"
		wait "$cachix_pid" || true
		exit "$status"
	}
	trap cachix_wait EXIT
fi


NIX_PIN="$(nix-build --no-out-link '<nixpkgs>' -A 'nix-pin')/bin/nix-pin"
export NTH_LINE=20
export SUMMARIZE='stderr'

set -x
"$NIX_PIN" create passe . --path nix/default.nix

function build {
	python <(curl -sSL 'https://gist.githubusercontent.com/timbertson/0fe86d8208146232bf0931a525cd9a9f/raw/long-output.py') \
		"$NIX_PIN" build --show-trace
	echo "== Built files:"
	find result/
}

function shell {
	python <(curl -sSL 'https://gist.githubusercontent.com/timbertson/0fe86d8208146232bf0931a525cd9a9f/raw/long-output.py') \
		"$NIX_PIN" shell "$@"
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
		build
		shell --pure --run "gup -x test"
		;;
	mirage-*)
		# no mirage tests yet; just check it builds
		;;
	*)
		echo "Error:  unknown $PASSE_TARGET"
		exit 1
		;;
esac
