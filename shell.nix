{pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	oc = ocamlPackages_4_01_0;
	ocScope = newScope oc;
in stdenv.mkDerivation {
	name="passe";
	buildInputs = [
		oc.ocaml
		python
		openssl
		oc.findlib
		oc.uri
		oc.yojson
		oc.js_of_ocaml
		oc.ocaml_lwt
		oc.ocaml_batteries
		oc.lambdaTerm
		oc.safepass
		(ocScope ./nix/ocaml-sha.nix {})
		(ocScope ./nix/cohttp.nix {
			magic-mime = ocScope ./nix/magic-mime.nix {};
			conduit = ocScope ./nix/conduit.nix {};
		})
	];
}
