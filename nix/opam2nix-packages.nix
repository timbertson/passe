{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/timbertson/opam2nix-packages.git";
		rev = "ee973cb6fcdabef7fc5c5b0cefbe7dd4064dbd6c";
		sha256 = "8a2ca27f8fcfcd130477a1062183dfce65c81307914c239e3f573f3eb70d9c84";
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
