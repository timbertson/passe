# sets $build_type to prod|dev
here="$(realpath --no-symlinks ${BASH_SOURCE[0]})"
gup -u "$here"
build_dir="$(basename "$(dirname "$here")")"
case $build_dir in
	_build) build_type="dev";;
	_build.prod) build_type="prod";;
	*)
		echo "Unknown build mode: $build_dir" >&2
		exit 2
		;;
esac
