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
		src = let
			passe = builtins.fetchGit { url = ./.; ref="HEAD"; };
			vdoml = builtins.fetchGit { url = ../vdoml; ref="HEAD"; };
		in {
			inherit vdoml;
			passe-client = passe;
			passe-server = passe;
			passe-common = passe;
			passe-unix-common = passe;
		};
	};
in
(api.build opamArgs).passe-server
