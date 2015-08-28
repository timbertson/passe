{
	pkgs,
	target,
	runTests,
}:
with pkgs;
let _target = target; in
let
	target = if _target == null then "common" else _target;
	opamDepsFile = (import ./opam-deps.nix {inherit target pkgs;});
	opamDeps = opamDepsFile.deps;
	opamSelections = opamDepsFile.selections;
	addGup = ''
		if ! which gup > /dev/null 2>&1; then
			export PATH="$PWD/tools/bin:$PATH"
		fi
	'';
	version = "TODO";

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

in stdenv.mkDerivation ({
	name="passe-v${version}";
	inherit target;

	src = fetchgit {
		# XXX use public location
		url = "/home/tim/dev/ocaml/passe";
		rev = "93ac7da0fdd17ca4a6444902d26948dd4fb4a934";
		sha256 = "0e96e974c11c2044ec03c92d10764dd93da6984c6bebe097011551f3f848279a";
	};

	shellHook = addGup;

	# buildPhase = "./tools/bin/gup " + (if runTests then "test" else "all");
	buildPhase = "true";
	stripDebugList = [ "_build" "_build.prod" ];
	installPhase = "./install.sh $out";

	LESSC = "${lessc}/bin/lessc";
	TWITTER_BOOTSTRAP = "${bootstrap}/lib/node_modules/bootstrap";
	NODE_PATH = lib.concatStringsSep ":"
		(map (base: "${base}/lib/node_modules") lessc_plugins);
	MARKDOWN = "${pythonPackages.markdown}/bin/markdown_py";
	# passthru = { inherit opamSelections; };
	buildInputs = [
		coreutils
		python
		openssl
		which
	]
	++ opamDeps
	++ (if runTests then [opamSelections.ounit] else []);

	# # XXX this seems to be necessary for .byte targets only
	# # (but we like those during development / testing).
	# # Seems very fragile though.
	LD_LIBRARY_PATH = lib.concatStringsSep ":" (lib.remove null (lib.mapAttrsToList (name: loc:
		if builtins.isAttrs loc then "${loc}/lib/${name}" else null
	) opamSelections));
}
	# // (
	# if runTests then {
	# 	CAML_LD_LIBRARY_PATH = lib.concatStringsSep ":" [
	# 		"${oc.ocaml_lwt}/lib/ocaml/${ocVersion}/site-lib/lwt"
	# 		"${oc.cstruct}/lib/ocaml/${ocVersion}/site-lib/cstruct"
	# 		"${oc.ocaml_ssl}/lib/ocaml/${ocVersion}/site-lib/ssl"
	# 		"${oc.lambdaTerm}/lib/ocaml/${ocVersion}/site-lib/lambda-term"
	# 		"${oc.sha}/lib/ocaml/${ocVersion}/site-lib/sha"
	# 		"${oc.safepass}/lib/ocaml/${ocVersion}/site-lib/safepass"
	# 	];
	# } else {}
	# )
)
