if [ "${GUP_XTRACE:-0}" = 1 ]; then
	set -x
fi
env_here="$(dirname ${BASH_SOURCE[0]})"
gup -u "${BASH_SOURCE[0]}"
. "$env_here/build_type.sh"
. "$env_here/compile_mode.sh"

