{
	pkgs ? import <nixpkgs> {},
	defaultTarget ? "devel",
	target ? null,
}:
import ./nix/local.nix
	{ inherit pkgs; }
	{
		target = pkgs.lib.findFirst
			(t: t != "" && t != null) defaultTarget
			[target (builtins.getEnv "PASSE_TARGET")];
	}

