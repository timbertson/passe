{ pkgs, lib,
	opam2nix,
	vdoml,
	self,
}:
with pkgs;
let
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

	combinedShell = deps: mkShell (wwwVars // {
		buildInputs = lib.concatMap (dep:
			(dep.drvAttrs.buildInputs or []) ++
			(dep.drvAttrs.propagatedBuildInputs or [])
		) deps;
	});

	opamCommon = {
		inherit (ocaml-ng.ocamlPackages_4_08) ocaml;
		src = {
			passe = self;
			passe-server = self;
			passe-common = self;
			passe-unix-common = self;
			inherit vdoml;
		};
		override = {}:
		let patchedShebangs = super: super.overrideAttrs (o: {
			postPatch = (super.postPatch or "") + "\npatchShebangs tools";
		});
		in
		{
			passe-server = super: (patchedShebangs super).overrideAttrs (o: wwwVars);
			passe = patchedShebangs;
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
			"--define" "${vdoml}/vdoml.opam"
			../passe-server.opam
			../passe.opam
		];
	
	result = {
		inherit vdoml opam2nix resolve;

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
