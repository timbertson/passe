#!/bin/bash

set -eux
set -o pipefail
gup image
cluster="starter-us-west-1"
user="$(oc whoami)"
apikey="$(oc whoami -t)"
registry="registry.$cluster.openshift.com"
sudo docker login -u "$user" -p "$apikey" "$registry"
dest="$registry/passe/passe"
sudo docker tag "$(cat image)" "$dest"
sudo docker push "$dest"
