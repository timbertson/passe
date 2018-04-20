
{ pkgs ? import <nixpkgs> {}, opam2nix ? null}:
if opam2nix != null then opam2nix else (
	with pkgs;
	let
		src = fetchgit {
			"url" = "https://github.com/timbertson/opam2nix-packages.git";
			"fetchSubmodules" = true;
			"sha256" = "1siy7b82hfhmqzhzff5zmxzsdaw3xk0mpg2pliqhxjmii99sv2ky";
			"rev" = "b2c769ea4218ac7e8f6a7d3f8be49693ba6bca13";
		};
		opam2nixSrc = fetchgit {
			"url" = "https://github.com/timbertson/opam2nix.git";
			"fetchSubmodules" = true;
			"sha256" = "03myq1yhcfi0dilzrm43gzyiy3pqxpl2ja0hw8wma5yzxf40hlhj";
			"rev" = "db3228a5c49c184530f11f65a20621567135c327";
		};
	in
	callPackage "${src}/nix" {
		opam2nixBin = callPackage "${opam2nixSrc}/nix" {};
	}
)
