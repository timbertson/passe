gup -u "${BASH_SOURCE[0]}"
build_type="$(basename $PWD)"
build_type="${build_type##*.}"
if [ "$build_type" != "build" -a "$build_type" != "prod" ]; then
	echo "Unknown build mode: $build_type" >&2
	exit 2
fi
