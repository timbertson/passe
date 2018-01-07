#!/bin/bash
set -eu
base="$(dirname "$0")"

installed=0
targets="$(ls -1 "$base" | grep '\.install$' || true)"
if [ -z "$targets" ]; then
	echo "Error: nothing to install - try building an .install file first"
	exit 1
fi

echo "Installing: $targets"

mkdir -p "$1"
PREFIX="$(cd "$1" && pwd)"
cd "$base"
shift

for target in $targets; do
	echo "Installing: $target"
	opam-installer "$target" --prefix="$PREFIX" "$@"
done
