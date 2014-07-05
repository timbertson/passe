if [ "${GUP_XTRACE:-0}" = 1 ]; then
	set -x
fi
here="$(realpath --no-symlinks ${BASH_SOURCE[0]})"
gup -u "${BASH_SOURCE[0]}"
. "$(dirname "$here")/build_type.sh"
. "$(dirname "$here")/compile_mode.sh"

