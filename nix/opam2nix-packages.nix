{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	src = fetchgit {
		fetchSubmodules = false;
		url = "https://github.com/gfxmonk/opam2nix-packages.git";
		rev = "c40f21eeda4df22f8b8e700c58734c77fb3e9af2";
		sha256 = "84819b591a2474b8346b270721da03c0989c5c3fcb10581e29622369e8df3599";
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
