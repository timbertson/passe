{ pkgs, opam2nix }:
with pkgs;
opam2nix.buildOpamPackage rec {
	version = "0.0.1";
	name = "opam-installer-${version}";
	src = (nix-update-source.fetch ./opam-installer.json).src;
	opamFile = "opam-installer.opam";
	ocamlAttr = "ocaml_4_03";
}
