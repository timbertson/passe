{stdenv, fetchurl, ocaml, findlib, stringext, uri, cstruct, ocaml_ipaddr, ocaml_lwt}:

stdenv.mkDerivation {
  name = "ocaml-conduit";

  src = fetchurl {
    url = https://github.com/mirage/ocaml-conduit/archive/v0.8.2.tar.gz;
    sha256 = "1yxi8nmw74d5bzwxzn8ycmhyaf04110fdhqwqh6wwwha1j3jv5ln";
  };

  buildInputs = [ ocaml findlib stringext uri cstruct ocaml_ipaddr ocaml_lwt ];

  propagatedBuildInputs = [ ocaml_ipaddr cstruct ];

  createFindlibDestdir = true;
}
