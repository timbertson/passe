{pkgs ? import <nixpkgs> {}, target ? null}:
with pkgs;
let
	# XXX upstream these
	sandstormPackages = import /home/tim/dev/nix/sandstorm/deps.nix { inherit pkgs; };
in
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
	] ++ (
		if (builtins.getEnv "PASSE_SANDSTORM") == "1" then (
			with sandstormPackages; [
				sandstorm-spk
				vagrant-spk
				vagrant
			]
		) else []
	);
})
