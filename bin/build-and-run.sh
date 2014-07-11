#!/bin/bash
set -eu
(gup -u www && echo -- "www built") &

gup -u _build/native/service.byte &&
	exec ./_build/native/service.byte
