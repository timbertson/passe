{target, pkgs}:
with pkgs;
let
	opam2nix-packages = callPackage ./opam2nix-packages.nix {};
	names = import (./opam-deps + "/${target}.nix" );
	opamArgs = {
		packages = names;
		ocamlAttr = "ocaml_4_02";
		extraRepos = [ ../safepass-xen/opam-repo ];
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
					mirage-xen-posix = lib.overrideDerivation sels.mirage-xen-posix (o: {
						src = mirage-platform-src;
					});
					mirage-xen-ocaml = lib.overrideDerivation sels.mirage-xen-ocaml (o: {
						src = mirage-platform-src;
					});
					mirage-xen = lib.overrideDerivation sels.mirage-xen (o: {
						src = mirage-platform-src;
					});
					io-page = lib.overrideDerivation sels.io-page (o: {
						src = fetchgit {
							url = "https://github.com/gfxmonk/io-page";
							rev = "6c4d5634ca1f8e8df0de9ec50b366ec7bd74984e";
							sha256 = "94ec05ffd213733cbe7c92291d06ea30122c4d9fa0c1bf74a912306d22cd0526";
						};
					});
					tcpip = lib.overrideDerivation sels.tcpip (o: {
						src = fetchgit {
							url = "https://github.com/gfxmonk/mirage-tcpip";
							rev = "c386b6a4a198cb122dd0b1fcb00291a09c6b1e98";
							sha256 = "0c70a6cf658eac0248cb345934384dc51149d0893ddbb00915968deced1c6ea0";
						};
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

