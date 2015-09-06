deps: args:
import ./default.nix deps ({
	version = "devel";
	src = ./local.tgz;
} // args)
