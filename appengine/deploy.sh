#!/usr/bin/env nix-shell
#!nix-shell -p google-cloud-sdk -i bash
set -eux
exec gcloud app deploy app.yaml --project passe-225909 --quiet "$@"
