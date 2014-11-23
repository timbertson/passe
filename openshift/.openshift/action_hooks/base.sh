source $OPENSHIFT_CARTRIDGE_SDK_BASH
export PID_PATH="$OPENSHIFT_DATA_DIR/passe.pid"
export LOG_TAG="sgp"
export LOG_FILE="$OPENSHIFT_LOG_DIR/$LOG_TAG.log"
export SERVER_PORT=8080

# set -x

function get_pid() {
	# whoami >&2
	# lsof -P >&2
	# XXX `lsof` seems the right way to do this, but it's busted (can't determine port number in whatever context these scripts run)
	# pid="$(lsof -P -i :$SERVER_PORT | tail -n +2 | awk '{print $2}')"

	pid="$(pgrep server)"
	# echo "  (got pid: $pid)" >&2
	if [ -z "$pid" ]; then
		echo "No server running on port $SERVER_PORT" >&2
	else
		echo "Process pid: $pid" >&2
		echo "-$pid"
	fi
}

function is_running() {
	pid="$1"
	if [ -z "$pid" ]; then
		echo "No PID file found"
		return 1
	else
		if kill -s 0 -- "$pid"; then
			return 0
		else
			clear_pid_file
			return 1
		fi
	fi
}

function kill_session() {
	pid="$1"
	if [ -n "$pid" ]; then
		while is_running "$pid"; do
			echo "Killing pid $pid"
			kill -s TERM -- "$pid"
			sleep 2
		done
	fi
}

function clear_pid_file() {
	rm -f "$PID_PATH"
}
