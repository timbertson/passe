let defaultTarget = "devel"; in
{ pkgs, lib,
	nix-update-source,
	opam2nix ? import ./opam2nix-packages {},
	vdoml ? null,
	target ? null
}:

let targetArg = target; in
let target = pkgs.lib.findFirst (t: t != "" && t != null) defaultTarget [targetArg (builtins.getEnv "PASSE_TARGET")]; in
with pkgs;
let
	targetParams =
		let
			generic = target: {
				buildTargets = ["www" target];
				opamDepsFile = (import ./opam-deps.nix {inherit target pkgs opam2nix vdoml;});
				drvAttrs = wwwVars;
			};
		in
		{
			# client + server, plus local development utils
			devel = generic "devel" // {
				buildTargets = ["all"];
			};

			# client-only
			client = generic "client" // {
				buildTargets = ["client"];
				drvAttrs = {};
			};

			# unix server
			server = generic "server";

			# xen unikernel
			mirage-xen = generic "mirage-xen";

			# unix unikernel
			mirage-unix = generic "mirage-unix";

			# ukvm unikernel
			mirage-ukvm = generic "mirage-ukvm";
		};

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
		MARKDOWN = "${pythonPackages.markdown}/bin/markdown_py";
	};

	makeTarget = target: { buildTargets, opamDepsFile, drvAttrs }:
		stdenv.mkDerivation ({
			inherit (nix-update-source.fetch ./src.json) src;
			name = "passe-${target}";
			buildPhase = ''
				echo "building passe ${target} (gup ${lib.concatStringsSep " " buildTargets})..."
				gup ${lib.concatStringsSep " " buildTargets}
			'';
			stripDebugList = [ "_build" ];
			installPhase = "./install.sh $out";

			passthru = rec {
				inherit (opamDepsFile) opam2nix names selections selectionsFile vdoml;
				selectionNames = lib.attrNames selections;
			};
			buildInputs = [
				gup
				git
				coreutils
				python
				openssl
				which
			]
			++ opamDepsFile.deps;

			# # XXX this seems to be necessary for .byte targets only
			# # (but we like those during development / testing).
			# # Seems very fragile though.
			LD_LIBRARY_PATH = lib.concatStringsSep ":" (lib.remove null (lib.mapAttrsToList (name: loc:
				if builtins.isAttrs loc then "${loc}/lib/${name}" else null
			) opamDepsFile.selections));
		} // drvAttrs);

	targets = lib.mapAttrs makeTarget targetParams;
in
	lib.getAttr target targets
