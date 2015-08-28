{pkgs ? import <nixpkgs> {}}:
with pkgs;
lib.overrideDerivation (callPackage ./default.nix { defaultTarget = "devel"; }) (orig: {
	# add some dev utils
	buildInputs = orig.buildInputs ++ [
		nodePackages.npm2nix
		git
	];
})
