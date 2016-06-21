#!/bin/bash
set -eu
base="$(dirname "$0")"
build_dir="_build.prod"
function print_dest {
	echo " Installing: $1 -> $2"
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

gup -u "$base/$build_dir/manifest"

if [ "$#" -eq 0 ]; then
	echo ""
	echo "# Dry run: if you provide a destination" >&2
	echo "# I will install the following files:" >&2
	echo ""
	function print_dest {
		echo "$1 -> \$PREFIX/$2"
	}
	function copy_relative {
		print_dest "$@"
	}
	function link_build_file {
		print_dest "$@"
	}
else
	dest="$1"
	echo " Installing from $base/$build_dir into $dest" >&2

	function copy_relative {
		abs="$dest/$2"
		print_dest "$1" "$abs"
		mkdir -p "$(dirname "$abs")"
		cp --dereference "$base/$f" "$abs"
		if [ ! -L "$abs" ]; then
			chmod u+w "$abs"
		fi
	}

	function link_build_file {
		if [ -e "$base/$build_dir/$1" ]; then
			ln -sfn "$build_dir/$1" "$dest/$1"
		fi
	}
fi
(cat "$base/$build_dir/manifest") | while read f; do
	if [ -e "$base/$f" ]; then
		# chop builddir prefix, if present
		d="${f#$build_dir/}"
		case "$d" in
			bin/*) ;;
			share/*) ;;
			*)
				# put misc built files into share/www
				d="share/www/$d";;
		esac
		copy_relative "$f" "$d"
	fi
done
