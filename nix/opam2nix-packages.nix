{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/timbertson/opam2nix-packages.git";
		rev = "e6313cdad453608a3132be170622320e2b52d762";
		sha256 = "693210cd8324ea98adecffb8ac14b2101031b1989af510123e0d41988c67793e";
	};

	# We could leave this out and just use  above,
	# but that leads to mass-rebuilds every time the repo changes
	# (rather than only when opam2nix is updated)
	opam2nix = fetchgit {
		url = "https://github.com/timbertson/opam2nix.git";
		rev = "c097bf2aca01083dae8b20288ee53613a8cd2a95";
		sha256 = "593fe4ae9d9e3ac633ab10ab8ce1d4cd67ec8636f1d926d9025d06879b5f5a90";
	};
in
callPackage "${src}/nix" {} { inherit src opam2nix; }
