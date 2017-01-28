{target, pkgs}:
with pkgs;
let
	opam2nix-packages = callPackage ./opam2nix-packages.nix {};
	vdoml = callPackage ./vdoml.nix {};
	names = import (./opam-deps + "/${target}.nix" );
	opamArgs = {
		packages = names;
		ocamlAttr = "ocaml_4_03";
		extraRepos = [
			# ../safepass-xen/opam-repo
			vdoml.opam2nix.repo
		];
		overrides = {super, self}: {
			opamSelection = let
				mirage-platform-src = fetchgit {
					url = "https://github.com/timbertson/mirage-platform";
					rev = "623d1184278e929e35efad2b4ab86fccceae20de";
					sha256 = "d2a5427d2b9aeef236c1e4ce9ae0275c8e9db878ca202a93b8c4dd96598b3e50";
				};
				sels = super.opamSelection;
				commonOverrides = {
					safepass = lib.overrideDerivation sels.safepass (o: {
						src = if builtins.pathExists ("${builtins.getEnv "PWD"}/safepass-xen/nix/local.tgz")
							then ../safepass-xen/nix/local.tgz
							else fetchgit {
								url = "https://github.com/timbertson/ocaml-safepass.git";
								rev = "cfa8f2277435f1d085cb437f99f928c93d0b2933";
								sha256 = "099fd01a224c18930d262a75d895dbd9a0183fe6c6f17b96f8b1059db34c9680";
							};
					});

					lwt = lib.overrideDerivation sels.lwt (o: {
						# TODO: remove ncurses hack when https://github.com/ocaml/opam-repository/pull/6773 is resolved
						nativeBuildInputs = o.nativeBuildInputs ++ [ ncurses ];
					});
				};
				xenOverrides = {
					io-page = lib.overrideDerivation sels.io-page (o: {
						patches = [ ./io-page.diff ];
					});
				};
			in
			lib.overrideExisting sels (commonOverrides // (if target == "mirage-xen" then xenOverrides else {}));
		};
	};
in
rec {
	inherit names opam2nix-packages;
	deps = opam2nix-packages.build opamArgs;
	selectionsFile = opam2nix-packages.selectionsFileLax opamArgs;
	selections = opam2nix-packages.importSelectionsFile selectionsFile opamArgs;
}

