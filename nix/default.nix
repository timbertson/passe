{ pkgs, lib, stdenv, opam2nix ? null }:
with pkgs;
let opam2nixOverride = opam2nix; in
let
	sources = pkgs.callPackage ./sources.nix {};
	opam2nix = if opam2nixOverride == null then (pkgs.callPackage sources.opam2nix {}) else opam2nixOverride;
	self = sources.local { url = ../.; };
	darwinFramework = framework:
		lib.optional stdenv.isDarwin (lib.getAttr framework pkgs.darwin.apple_sdk.frameworks);
	wwwVars =
		let
			nodeEnv = let base = pkgs.callPackage ./node-env.nix {}; in base // {
				# never NPM install, it can only cause you sadness
				buildNodePackage = args: base.buildNodePackage (args // { dontNpmInstall = true; });
			};
			npm_deps = (pkgs.callPackage ./npm-deps.nix { inherit nodeEnv; });
			nodePath = pkg: "${pkg}/lib/node_modules";
			bootstrap = npm_deps."bootstrap-3.2.0";
			# lesscPath = "bin/lessc";
			lesscPath = "lib/node_modules/less/bin/lessc"; # This is weird, no idea if it's temporary :shrug:
		in with npm_deps; {
		LESSC = "${less}/${lesscPath}";
		TWITTER_BOOTSTRAP = "${nodePath bootstrap}/bootstrap";
		NODE_PATH = lib.concatMapStringsSep ":" nodePath [ less less-plugin-clean-css bootstrap ];
		MARKDOWN = "${python3Packages.markdown}/bin/markdown_py";
	};

	src = null;

	devTools = [
		selection.utop

		# temporarily not compiling; TODO pick a working version?
		# selection.ocaml-lsp-server

		selection.dune
		pkgs.patchelf
	];

	combinedShell = deps: mkShell (wwwVars // {
		buildInputs = lib.concatMap (dep:
			(dep.drvAttrs.buildInputs or []) ++
			(dep.drvAttrs.propagatedBuildInputs or [])
		) deps ++ devTools;
	});

	opamCommon = {
		inherit (ocaml-ng.ocamlPackages_4_12) ocaml;
		src = {
			passe = self;
			passe-server = self;
			passe-common = self;
			passe-unix-common = self;
			inherit (sources) vdoml;
		};
		override = {}:
			let patchedShebangs = super: super.overrideAttrs (o: {
				postPatch = (super.postPatch or "") + "\npatchShebangs tools";
			});
			in
			{
				passe-server = super: (patchedShebangs super).overrideAttrs (o: wwwVars);
				passe = patchedShebangs;

				dune = super: super.overrideAttrs (o: {
					buildInputs = (o.buildInputs or []) ++ (darwinFramework "CoreServices");
				});
				
				utop = super: super.overrideAttrs (o: {
					sourceRoot = ".";
				});
			};
	};

	importSelection = selection:
		opam2nix.build (opamCommon // {
			inherit selection;
		});

	selection = importSelection ./opam-selection.nix;
	mirageUnixSelection = importSelection ./opam-selection-mirage-unix.nix;
	mirageXenSelection = importSelection ./opam-selection.-mirage-xen;

	resolve = { selection }:
		opam2nix.resolve (opamCommon // { inherit selection; }) [
			"--define" ../passe-common.opam
			"--define" ../passe-unix-common.opam
			"--define" "${sources.vdoml}/vdoml.opam"
			../passe-server.opam
			../passe.opam
			"utop"
			"ocaml-lsp-server"
		];
	
	result = {
		inherit vdoml opam2nix resolve nix-wrangle;

		# client + server, plus local development utils
		shell = combinedShell [
			selection.passe
			selection.passe-server
		];

		inherit selection;
		inherit (selection) passe passe-server;
		# TODO: get these working
		# mirage-unix = mirageUnixSelection;
		# mirage-xen = mirageXenSelection;
	};
in
result.passe.overrideAttrs (super: {
	passthru = result;
})
