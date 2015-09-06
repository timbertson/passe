{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/gfxmonk/opam2nix-packages.git";
		rev = "5f477685193c5132de224f52caf82cc5c0074319";
		sha256 = "e62caf3c9e833d4a258f74dd54c60e3e23d37c6c5dedacb854cee0eb1942c054";
	};

	# We could leave this out and just use `fetchSubmodules` above,
	# but that leads to mass-rebuilds every time the repo changes
	# (rather than only when opam2nix is updated)
	opam2nix = fetchgit {
		url = "https://github.com/gfxmonk/opam2nix.git";
		rev = "5c981b597007bd781fed48c03951a5a18bb6ee46";
		sha256 = "6e7fae31d6aa8ab69653862e6c0c9e0bd535db8885cd3027b581c27ab992e895";
	};
in
callPackage "${src}/nix" {} { inherit src opam2nix; }
