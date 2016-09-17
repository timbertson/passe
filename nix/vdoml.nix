{ pkgs ? import <nixpkgs> {}}:
with pkgs; let
	devRepo = builtins.getEnv "VDOML_DEVEL";
	src = fetchgit {
		"url" = "https://github.com/timbertson/vdoml.git";
		"sha256" = "TODO";
		"rev" = "TODO";
	};
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {
		inherit pkgs;
		src = toPath "${devRepo}/nix/local.tgz";
	}
else callPackage "${src}/nix" {} { inherit pkgs src; }
