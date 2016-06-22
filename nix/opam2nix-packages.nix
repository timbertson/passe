{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/timbertson/opam2nix-packages.git";
		rev = "d30f9bfde144a29d16248b7369d609268d6280ad";
		sha256 = "84b9941377f7b8b317beffd70ed0e7b50b992f6340cac6a514208ee6924210a1";
	};

	# We could leave this out and just use `fetchSubmodules` above,
	# but that leads to mass-rebuilds every time the repo changes
	# (rather than only when opam2nix is updated)
	opam2nix = fetchgit {
		url = "https://github.com/timbertson/opam2nix.git";
		rev = "f438e20ea99d5dccae9721a4311cc0d0d68cb6bb";
		sha256 = "6691a3fa6d86d7ba64d66ac9e14345d900af44b8c692d30e6b318e793b710d07";
	};
in
callPackage "${src}/nix" {} { inherit src opam2nix; }
