{ pkgs }:
{ src,
	version,
	target,
}:
with pkgs;
let
	# possible targets:
	# client (client-only)
	# server (unix server)
	# mirage-xen (xen mirage microkernel)
	# mirage-unix (unix mirage microkernel, mainly for testing)
	# devel (client + server, plus local development utils)

	buildDir = "_build.prod";
	buildTargets = assert (target != "" && target != null); let build = target: "${buildDir}/${target}"; in
		if target == "devel" then [ (build "all") ]
		else if target == "client" then [ (build target) ]
		else [ (build "www") (build target) ];

	opamDepsFile = (import ./opam-deps.nix {inherit target pkgs;});
	opamDeps = opamDepsFile.deps;
	opamSelections = opamDepsFile.selections;

	commonAttrs = {
		inherit src;
		name = "passe-${target}-${version}";
		shellHook = ''
			if ! which gup > /dev/null 2>&1; then
				export PATH="$PWD/tools/bin:$PATH"
			fi
		'';

		preConfigure = commonAttrs.shellHook;
		buildPhase = ''
			echo "building passe ${target} (gup ${lib.concatStringsSep " " buildTargets})..."
			gup ${lib.concatStringsSep " " buildTargets}
		'';
		stripDebugList = [ "_build.prod" ];
		installPhase = "./install.sh ${buildDir} $out";

		passthru = {
			inherit opamSelections;
			selectionNames = lib.attrNames opamSelections;
		};
		buildInputs = [
			coreutils
			python
			openssl
			which
		]
		++ opamDeps;

		# # XXX this seems to be necessary for .byte targets only
		# # (but we like those during development / testing).
		# # Seems very fragile though.
		LD_LIBRARY_PATH = lib.concatStringsSep ":" (lib.remove null (lib.mapAttrsToList (name: loc:
			if builtins.isAttrs loc then "${loc}/lib/${name}" else null
		) opamSelections));
	};

in

stdenv.mkDerivation (commonAttrs // (
	if target == "client" then {} else let
		npm_deps = pkgs.callPackage ./npm-deps.nix {
			self = npm_deps // {
				buildNodePackage = nodePackages.buildNodePackage;
			};
		};
		bootstrap = npm_deps.by-spec.bootstrap."3.2.0";
		lessc = npm_deps.by-spec.less."*";
		lessc_plugins = [
			npm_deps.by-spec."less-plugin-clean-css"."*"
		];

	in ({
		LESSC = "${lessc}/bin/lessc";
		TWITTER_BOOTSTRAP = "${bootstrap}/lib/node_modules/bootstrap";
		NODE_PATH = lib.concatStringsSep ":"
			(map (base: "${base}/lib/node_modules") lessc_plugins);
		MARKDOWN = "${pythonPackages.markdown}/bin/markdown_py";
	})
))
