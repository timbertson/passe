#!/bin/bash
set -eu
(gup -u www && echo -- "www built") &

gup -u _build/native/bin/server.byte &&
	exec ./_build/native/bin/server.byte
