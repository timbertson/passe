# sets $compile_mode to js/native
gup -u "${BASH_SOURCE[0]}"
compile_mode="${2%%/*}"
if [ "$compile_mode" != "native" -a "$compile_mode" != "js" ]; then
	echo "Unknown compile_mode: $compile_mode" >&2
	exit 2
fi

