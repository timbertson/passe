# sets $build_type to prod|dev
self="$(realpath --no-symlinks ${BASH_SOURCE[0]})"
gup -u "$self"
build_dir="$(basename "$(dirname "$self")")"
case $build_dir in
	_build) build_type="dev";;
	_build.prod) build_type="prod";;
	*)
		echo "Unknown build mode: $build_dir" >&2
		exit 2
		;;
esac
