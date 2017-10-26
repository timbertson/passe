{
	pkgs ? import <nixpkgs> {},
	defaultTarget ? "devel",
	target ? null,
	opam2nix ? import ./opam2nix-packages.nix {},
}:
import ./default.nix
	{ inherit pkgs opam2nix; }
	{
		version = "devel";
		src = ./local.tgz;
		target = pkgs.lib.findFirst
			(t: t != "" && t != null) defaultTarget
			[target (builtins.getEnv "PASSE_TARGET")];
	}

