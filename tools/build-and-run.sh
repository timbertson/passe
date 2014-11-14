#!/bin/bash
set -eu
cd ..
(gup -u www && echo -- "www built") &

exec tools/passe-server
