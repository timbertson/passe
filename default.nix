{
	pkgs ? import <nixpkgs> {},
	defaultTarget ? null,
	target ? null,
}:
import ./nix/local.nix
	{ inherit pkgs; }
	{
		target = pkgs.lib.findFirst
			(t: t != "" && t != null) defaultTarget
			[target (builtins.getEnv "PASSE_TARGET")];
	}

