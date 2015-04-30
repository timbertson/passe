{stdenv, fetchurl, ocaml, findlib, shared_mime_info}:

stdenv.mkDerivation {
  name = "ocaml-magic-mime";

  src = fetchurl {
    url = https://github.com/mirage/ocaml-magic-mime/archive/v1.0.0.tar.gz;
    sha256 = "0ng1a5x49rd9zb432vhf3cmqmsi9rf4v441a8rqbv3bvdr3p5bm9";
  };

  buildInputs = [ ocaml findlib ];

  configureFlags=["--prefix=${shared_mime_info}"];

  createFindlibDestdir = true;
}
