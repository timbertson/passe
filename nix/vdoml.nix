{ pkgs ? import <nixpkgs> {}}:
with pkgs; let
	devRepo = builtins.getEnv "VDOML_DEVEL";
	src = nix-update-source.fetch ./vdoml.json;
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {
		inherit pkgs;
		src = toPath "${devRepo}/nix/local.tgz";
	}
else callPackage "${src}/nix" {} { inherit pkgs src; }
