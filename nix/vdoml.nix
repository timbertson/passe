{ pkgs, opam2nix, ocamlAttr }:
with pkgs; let
	src = (nix-update-source.fetch ./vdoml.json).src;
in
callPackage "${src}/nix" { inherit pkgs opam2nix ocamlAttr; }
