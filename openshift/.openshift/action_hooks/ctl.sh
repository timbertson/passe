set -eu
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

	pid="$(pgrep -f passe-server)"
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

function stop_action {
	echo " *** Ensuring app is stopped..."
	pid="$(get_pid)"
	kill_session "$pid"
}

function start_action {
	stop_action
	echo "*** Starting app ..."
	{ nohup setsid bash -eu <<'EOF'
		echo -e "\n---------------------------------\n*** Start: $(date) - PID $$"
		echo $$ > $PID_PATH
		# note: we need to explicitly invoke lib/ld because RHEL's ld-linux.so is usually too old
		set -x
		export LD_LIBRARY_PATH="$OPENSHIFT_REPO_DIR/lib"
		export OCAMLRUNPARAM=b
		exec $OPENSHIFT_REPO_DIR/lib/ld.so \
			$OPENSHIFT_REPO_DIR/app/bin/passe-server \
			--port $SERVER_PORT \
			--host $OPENSHIFT_DIY_IP \
			--root $OPENSHIFT_REPO_DIR/docroot \
			--data $OPENSHIFT_DATA_DIR \
		;
EOF
	} |& /usr/bin/logshifter -tag sgp &

	echo "*** Waiting for app startup ..."
	sleep 2
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
