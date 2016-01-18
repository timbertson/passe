{pkgs ? import <nixpkgs> {}, target ? null}:
with pkgs;
lib.overrideDerivation (
		callPackage ./default.nix {
			inherit target;
		}
	) (orig: {
	# add some dev utils
	buildInputs = orig.buildInputs ++ [
		orig.opam2nix
		nodePackages.npm2nix
		git
	];
})
