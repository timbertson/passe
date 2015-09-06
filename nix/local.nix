deps: args:
with deps.pkgs;
callPackage ./default.nix {} ({
	version = "devel";
	src = ./local.tgz;
} // args)
