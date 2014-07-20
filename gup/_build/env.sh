if [ "${GUP_XTRACE:-0}" = 1 ]; then
	set -x
fi
env_here="$(realpath --no-symlinks ${BASH_SOURCE[0]})"
gup -u "${BASH_SOURCE[0]}"
. "$(dirname "$env_here")/build_type.sh"
. "$(dirname "$env_here")/compile_mode.sh"

