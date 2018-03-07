{ pkgs, opam2nix, ocamlAttr }:
with pkgs;
opam2nix.buildOpamPackage rec {
	version = "0.0.1";
	name = "opam-installer-${version}";
	inherit ocamlAttr;
	src = (nix-update-source.fetch ./opam-installer.json).src;
	opamFile = "opam-installer.opam";
}
