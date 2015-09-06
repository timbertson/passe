{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/gfxmonk/opam2nix-packages.git";
		rev = "7104f035c734171853ee38ccba4140c456d035c3";
		sha256 = "8da29a5753fd0fb338d1dae76887fa4d02232950104bf4e099d534b3ba56a0de";
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
