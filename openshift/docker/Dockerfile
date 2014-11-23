from centos:centos6
# http://software.opensuse.org/download.html?project=home%3Aocaml&package=opam
run bash -euxc 'cd /etc/yum.repos.d/; curl --fail -L -O http://download.opensuse.org/repositories/home:ocaml/CentOS_6/home:ocaml.repo'
run yum -y install ocaml opam python
run yum install -y git
run yum install -y m4
run yum install -y tar
run yum install -y ocaml-ocamldoc binutils
run yum install -y ocaml-camlp4 which
run yum install -y ocaml-camlp4-devel
run yum install -y openssl-devel

# XXX these match my uid/gid, but that's hacky
run groupadd app --gid=1001
run useradd app --uid=1000 --gid=1001

# now set up app user environment
user app
run opam init --auto-setup
run OPAMYES=1 opam install camlp4
run OPAMYES=1 opam install batteries yojson ssl cohttp lambda-term uri lwt sha safepass js_of_ocaml
