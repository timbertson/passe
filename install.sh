#!/bin/bash
set -eu
base="$(dirname "$0")"
build_dir="_build.prod"
function print_dest {
	echo " Installing: $1"
}

if [ "$#" -gt 1 ]; then
	case "$1" in
		_build*)
			build_dir="$1"
			;;
		*)
			echo "Expected build directory, got $1"
			exit 1
			;;
	esac
	shift 1
fi

gup -u "$build_dir/manifest"

if [ "$#" -eq 0 ]; then
	echo ""
	echo "# Dry run: if you provide a destination" >&2
	echo "# I will install the following files:" >&2
	echo ""
	function print_dest {
		echo "$1"
	}
	function copy_relative {
		print_dest "$1"
	}
	function link_build_file {
		print_dest "$1"
	}
else
	dest="$1"
	echo " Installing from $base/$build_dir into $dest" >&2

	function copy_relative {
		print_dest "$1"
		abs="$dest/$f"
		mkdir -p "$(dirname "$abs")"
		cp -a "$base/$f" "$abs"
		if [ ! -L "$abs" ]; then
			chmod u+w "$abs"
		fi
	}

	function link_build_file {
		if [ -e "$build_dir/$1" ]; then
			ln -sfn "$build_dir/$1" "$dest/$2"
		fi
	}
fi
cat "$base/$build_dir/manifest" | while read f; do
	if [ -e "$f" ]; then
		copy_relative "$f"
	fi
done
link_build_file  "native/bin" "bin"
