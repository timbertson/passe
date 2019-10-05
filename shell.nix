with import <nixpkgs> {};
let
	nix-wrangle = callPackage /Users/tcuthbertson/Code/nix/nix-wrangle/nix/default.nix {};
	opam2nix = callPackage /Users/tcuthbertson/Code/ocaml/opam2nix/default.nix {
		inherit nix-wrangle;
	};
	ocaml = ocaml-ng.ocamlPackages_4_08.ocaml;
	api = opam2nix.api { inherit opam2nix; }; # cheat
	opamArgs = {
		deps = ./opam-packages.nix;
		inherit ocaml;
		src = builtins.fetchGit { url = ./.; ref="HEAD"; };
	};
in
stdenv.mkDerivation {
	name = "test";
	buildInputs = api.buildInputs opamArgs;
	passthru = api.build opamArgs;
}
