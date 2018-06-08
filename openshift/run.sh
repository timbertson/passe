#!/bin/sh
set -eux
export LD_LIBRARY_PATH="/app/lib:${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH}"
exec /app/lib/ld.so \
	/app/bin/passe-server \
	--host 0.0.0.0 \
	--data /data \
	--timestamp \
	-v \
	;
