gup -u "${BASH_SOURCE[0]}"
build_type="$(basename $PWD)"
build_type="${build_type##*.}"
if [ "$build_type" != "_build" -a "$build_type" != "prod" ]; then
	echo "Unknown build mode: $build_type" >&2
	exit 2
fi
compile_mode="${2%%/*}"
if [ "$compile_mode" != "native" -a "$compile_mode" != "js" ]; then
	echo "Unknown compile_mode: $compile_mode" >&2
	exit 2
fi

