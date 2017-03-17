{
	pkgs ? import <nixpkgs> {},
	defaultTarget ? "devel",
	target ? null,
	opam2nix ? import ./nix/opam2nix-packages.nix {},
}:
import ./nix/local.nix
	{ inherit pkgs opam2nix; }
	{
		target = pkgs.lib.findFirst
			(t: t != "" && t != null) defaultTarget
			[target (builtins.getEnv "PASSE_TARGET")];
	}

