set -eu
source $OPENSHIFT_CARTRIDGE_SDK_BASH
export PID_PATH="$OPENSHIFT_DATA_DIR/passe.pid"
export LOG_TAG="sgp"
export LOG_FILE="$OPENSHIFT_LOG_DIR/$LOG_TAG.log"
export SERVER_PORT=8080
export PYTHONUNBUFFERED=1

# set -x

function is_running() {
	pid="$1"
	if [ -z "$pid" ]; then
		return 1
	else
		if kill -s 0 -- "$pid" 2>/dev/null; then
			return 0
		else
			clear_pid_file
			return 1
		fi
	fi
}

function clear_pid_file() {
	rm -f "$PID_PATH"
}

function get_pid() {
	pid="$(cat $PID_PATH 2>/dev/null)"
	if [ -n "$pid" ]; then
		if is_running $pid; then
			echo "Process pid: $pid" >&2
			pid="-$pid"
		else
			pid=""
			clear_pid_file
		fi
	else
		# no pidfile; check whether it's running anyway
		pid="$(pgrep -f passe-server)"
	fi
	# echo "  (got pid: $pid)" >&2
	if [ -n "$pid" ]; then
		echo "$pid"
	else
		echo "No server running on port $SERVER_PORT" >&2
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

function stop_action {
	echo " *** Ensuring app is stopped..."
	pid="$(get_pid)"
	kill_session "$pid"
}

function start_action {
	stop_action
	echo "*** Starting app ..."
	{ nohup setsid python "$OPENSHIFT_REPO_DIR/run.py"
	} |& logshifter -tag "$LOG_TAG" &

	echo "*** Waiting for app startup ..."
	sleep 4
	pid="$(get_pid)"
	if ! is_running "$pid"; then
		echo -e "*** Process failed! Recent logs:\n\n"
		tail -n40 "$LOG_FILE" | sed -e 's/^/  # /'
		exit 1
	fi
}

if [ "$#" -gt 0 ]; then
	case $1 in
		start) start_action;;
		stop) stop_action;;
		*) echo "ERROR: unknown action $1"; exit 1;;
	esac
fi
