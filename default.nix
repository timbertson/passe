{
	pkgs ? import <nixpkgs> {},
	defaultTarget ? null,
	target ? let m = builtins.getEnv "PASSE_TARGET"; in if m == "" then defaultTarget else m,
	runTests ? false,
}:
import ./nix/local.nix {
	inherit pkgs target runTests;
}

