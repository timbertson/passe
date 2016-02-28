# sets $build_type to prod|dev
self="${BASH_SOURCE[0]}"
gup -u "$self"
build_dir="$(basename "$(dirname "$self")")"

# XXX workaround for gup bug, fixed in 0.5.4
build_dir="$(pwd | grep -E -o '_build[^/]*?' | tail -n 1)"

case $build_dir in
	_build) build_type="dev";;
	_build.prod) build_type="prod";;
	*)
		echo "Unknown build mode: $build_dir" >&2
		exit 2
		;;
esac
