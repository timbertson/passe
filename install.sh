#!/bin/bash
set -eu
base="$(dirname "$0")"
cd "$base"
PREFIX="$1"
shift

installed=0
targets="$(ls -1 | grep '\.install$' || true)"
if [ -z "$targets" ]; then
	echo "Error: nothing to install - try building an .install file first"
	exit 1
fi

echo "Installing: $targets"
for target in $targets; do
	echo "Installing: $target"
	opam-installer "$target" --prefix="$PREFIX" "$@"
done
