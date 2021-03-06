# sets $compile_mode to js/native
gup -u "${BASH_SOURCE[0]}"
compile_mode="${2#*/}"
compile_mode="${compile_mode#*/}"
compile_mode="${compile_mode%%/*}"
if [ \
	"$compile_mode" != "native" \
	-a "$compile_mode" != "js" \
	-a "$compile_mode" != "mirage-unix" \
	-a "$compile_mode" != "mirage-xen" \
	]; then
	echo "Unknown compile_mode: $compile_mode" >&2
	exit 2
fi

