{stdenv, fetchurl, ocaml, findlib }:

stdenv.mkDerivation {
  name = "ocaml-sha";

  src = fetchurl {
    url = https://github.com/vincenthz/ocaml-sha/archive/ocaml-sha-v1.9.tar.gz;
    sha256 = "1l48l310cl17jz8lxv787plbbhsahbhvh6q6h2hnrif2f68dv8fa";
  };

  buildInputs = [ ocaml findlib ];

  createFindlibDestdir = true;
}
