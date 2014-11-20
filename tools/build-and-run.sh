#!/bin/bash
set -eu
cd "$(dirname "$0")"/..
(gup -u www && echo -- "www built") &

exec tools/server
