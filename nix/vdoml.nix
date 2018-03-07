{ pkgs, opam2nix, ocamlAttr }:
with pkgs; let
	devRepo = builtins.getEnv "VDOML_DEVEL";
	src = (nix-update-source.fetch ./vdoml.json).src;
	attrs = {inherit pkgs opam2nix ocamlAttr; };
in
if devRepo != "" && builtins.pathExists devRepo then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" (attrs // {
		src = toPath "${devRepo}/nix/local.tgz";
	})
else callPackage "${src}/nix" (attrs // { inherit src; })
