#!/bin/bash
set -eu
base="$(dirname "$0")"
cd "$base"

installed=0
ls -1 | grep '\.install' | while read f; do
	echo "Installing: $f"
	opam-installer "$f" --prefix="$PREFIX" "$@"
	((installed++))
done
if [ "$installed" = 0 ]; then
	echo "Error: nothing to install - try building an .install file first"
	exit 1
fi
