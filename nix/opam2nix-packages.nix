{ pkgs ? import <nixpkgs> {}}:
with pkgs;
rec {
	src = fetchgit {
		url = "https://github.com/gfxmonk/opam2nix-packages.git";
		rev = "6f8c46e88c71c5876ff1b02f70085616fbdcc126";
		sha256 = "0dg4niisixwzldc1dyjvwmrnppbn3qlcl86w1gykg97bjrw5gm85";
	};
	"import" = import "${src}/import.nix";
	opam2nix = lib.overrideDerivation
		(callPackage "${src}/opam2nix/nix/opam2nix.nix" {})
		(base: { src = "${src}/opam2nix"; });
}
