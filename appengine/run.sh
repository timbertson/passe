#!/bin/sh
set -eux
here="$(dirname "$0")"
base="$here/app"
exec "$base/lib/ld.so" \
	"$base/bin/passe-server" \
	--host 0.0.0.0 \
	--timestamp \
	--port "$PORT" \
	--cloud-datastore secrets/passe.json \
	${PASSE_FLAGS:-} \
	"$@"
	;
