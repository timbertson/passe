{target, pkgs, opam2nix }:
with pkgs;
let
	vdoml = callPackage ./vdoml.nix { inherit opam2nix; };
	names = import (./opam-deps + "/${target}.nix" );
	opamArgs = {
		packages = names;
		ocamlAttr = "ocaml_4_03";
		extraRepos = [
			vdoml.opam2nix.repo
		];
		# args = [ "--verbose" "--repo"
		# 	../../ocaml-safepass/opam2nix-repo
		# ];
		extraPackages = [
			(import ../../ocaml-safepass/opam2nix-repo/packages)
		];

		overrides = {super, self}: {
			opamSelection = let
				sels = super.opamSelection;

				# TODO: upstream
				disableHardening = pkg: lib.overrideDerivation pkg (o: {
					hardeningDisable = ["stackprotector" "format"];
				});

				commonOverrides = {
					safepass = disableHardening (lib.overrideDerivation sels.safepass (o: {
						buildInputs = o.buildInputs ++ [ pkgs.pkgconfig ];
						src = ../../ocaml-safepass/nix/local.tgz;
						# patches = [ ../../ocaml-safepass/mirage.diff ];
						# configurePhase = ''
						# 	sed -i -e 's/has_native_dynlink\ *:.*/has_native_dynlink:false/' setup.ml
						# '';
						installPhase = o.installPhase + '';
							echo 'freestanding_linkopts = "-lsafepass_stubs"' >> $out/lib/safepass/META
						'';
					}));

					mirage = lib.overrideDerivation sels.mirage (o: {
						src = fetchgit {
							"url" = "https://github.com/timbertson/mirage.git";
							"rev" = "4aee6d57e02fc0e7adaafae185fb18efab12e640";
							"sha256" = "1n54chakxamqg8irhj8mw8f21g29d8z2xi7vwhczlva2z9m4zxwx";
						};
					});

					nocrypto = disableHardening sels.nocrypto;
					mirage-solo5 = disableHardening sels.mirage-solo5;
					mirage-entropy = disableHardening sels.mirage-entropy;

					ocaml-freestanding = disableHardening (lib.overrideDerivation sels.ocaml-freestanding (o: {
						configurePhase = ''
							sed -i -e '/export PKG_CONFIG_PATH/d' configure.sh
							sed -i -e 's/cp .*ocamlfind query ocaml-src.*/\0; chmod -R u+rwX build/' Makefile
						'';
					}));

					zarith-freestanding = disableHardening (lib.overrideDerivation sels.zarith-freestanding (o: {
						configurePhase = ''
							sed -i -E -e '/PKG_CONFIG_PATH|opam config var prefix/d' mirage-build.sh
							sed -i -e 's|`opam config var prefix`|"$out"; mkdir -p "$out/lib/zarith/"|' mirage-install.sh
							sed -i -e 's|lib/zarith/|lib/zarith-freestanding/|' mirage-install.sh
						'';
					}));

					gmp-freestanding = disableHardening (lib.overrideDerivation sels.gmp-freestanding (o: {
						installPhase = o.installPhase + '';
							touch $OCAMLFIND_DESTDIR/gmp-freestanding/META
						'';
					}));

					ocb-stubblr = lib.overrideDerivation sels.ocb-stubblr (o: {
						# TODO: https://github.com/pqwy/ocb-stubblr/pull/10
						patches = [ ./stubblr.patch ];
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
	inherit names opam2nix vdoml;
	deps = opam2nix.build opamArgs;
	selectionsFile = opam2nix.selectionsFileLax opamArgs;
	selections = opam2nix.importSelectionsFile selectionsFile opamArgs;
}

