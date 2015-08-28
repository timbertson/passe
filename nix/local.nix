args:
with args.pkgs;
lib.overrideDerivation (import ./passe.nix args) (orig: {
	name="passe-local";
	src = ./local.tgz;
})
