{pkgs ? import <nixpkgs> {}, target ? null}:
with pkgs;
lib.overrideDerivation (
		callPackage ./default.nix {
			inherit target;
			defaultTarget = "devel";
		}
	) (orig: {
	# add some dev utils
	buildInputs = orig.buildInputs ++ [
		nodePackages.npm2nix
		git
	];
})
