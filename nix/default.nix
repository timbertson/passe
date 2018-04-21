{ pkgs, opam2nix }:
{ src,
	version,
	target,
}:
with pkgs;
let
	targetParams =
		let
			generic = target: {
				buildTargets = ["www" target];
				opamDepsFile = (import ./opam-deps.nix {inherit target pkgs opam2nix;});
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
			nodeEnv = pkgs.callPackage ./node-env.nix {};
			npm_deps = (pkgs.callPackage ./npm-deps.nix { inherit nodeEnv; });
			nodePath = pkg: "${pkg}/lib/node_modules";
			bootstrap = npm_deps."bootstrap-3.2.0";
		in with npm_deps; {
		LESSC = "${less}/bin/lessc";
		TWITTER_BOOTSTRAP = "${nodePath bootstrap}/bootstrap";
		NODE_PATH = lib.concatMapStringsSep ":" nodePath [ less less-plugin-clean-css bootstrap ];
		MARKDOWN = "${pythonPackages.markdown}/bin/markdown_py";
	};

	makeTarget = target: { buildTargets, opamDepsFile, drvAttrs }:
		stdenv.mkDerivation ({
			inherit src;
			name = "passe-${target}-${version}";
			buildPhase = ''
				# hacky workaround for https://github.com/janestreet/jbuilder/issues/298:
				# build the file, then pop it into the source tree.
				# In the future jbuilder may complain about this, and we'll need to remove the version.ml rule entirely :(
				gup _build/default/src/common/version.ml
				cp _build/default/src/common/version.ml src/common/version.ml

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
