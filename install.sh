#!/bin/bash
set -eu
base="$(dirname "$0")"

PREFIX="$(realpath "$1")"
shift

install_base="_build/default"

if [ "$#" -eq 0 ]; then
	targets="$(cd "$base/$install_base" && find -maxdepth 1 -type f -name '*.install')"
	if [ -z "$targets" ]; then
		echo "Error: nothing to install - try building an .install file first"
		exit 1
	fi
else
	targets="$@"
fi

mkdir -p "$PREFIX"
prefix="$(realpath "$PREFIX")"

for target in $targets; do
	echo "Installing: $target"
	pwd
	cd "$base"
	opam-installer "$install_base/$target" --prefix="$PREFIX"
done
