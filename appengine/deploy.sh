#!/bin/bash
set -eux
exec gcloud app deploy app.yaml --project passe-225909 --quiet "$@"
