{target, pkgs ? null}:
let
	# XXX is repetitive; could surely be neater...
	names = import (
		if target == "common" then ./opam-deps/common.nix
		else if target == "mirage-unix" then ./opam-deps/mirage-unix.nix
		else if target == "mirage-xen" then ./opam-deps/mirage-xen.nix
		else if target == "devel" then ./opam-deps/devel.nix
		else assert false; null
	);
	selections = assert pkgs != null;
		import (
			if target == "common" then ./selections.common.nix
			else if target == "mirage-unix" then ./selections.mirage-unix.nix
			else if target == "mirage-xen" then ./selections.mirage-xen.nix
			else if target == "devel" then ./selections.devel.nix
			else assert false; null
		) {
			inherit pkgs;
			opamPackages = import ../../opam2nix/dest/nix {inherit pkgs; };
			opam2nix = pkgs.callPackage ../../opam2nix/nix/local.nix {};
			# for testing patches...
			overrideSelections = sels: if target == "mirage-xen" then sels // {
				mirage-xen-posix = pkgs.lib.overrideDerivation sels.mirage-xen-posix (o: {
					src = /home/tim/dev/scratch/mirage-platform/local.tgz;
				});
				mirage-xen-ocaml = pkgs.lib.overrideDerivation sels.mirage-xen-ocaml (o: {
					src = /home/tim/dev/scratch/mirage-platform/local.tgz;
				});
				mirage-xen = pkgs.lib.overrideDerivation sels.mirage-xen (o: {
					src = /home/tim/dev/scratch/mirage-platform/local.tgz;
				});
				io-page = pkgs.lib.overrideDerivation sels.io-page (o: {
					src = /home/tim/dev/scratch/io-page/local.tgz;
				});
				tcpip = pkgs.lib.overrideDerivation sels.tcpip (o: {
					src = /home/tim/dev/scratch/mirage-tcpip/local.tgz;
				});
			} else sels;
		};
in
{
	inherit names selections;
	# deps is just a flat list of every "root" package
	# (i.e the implementation of each named package in ./opam-deps-{target}.nix)
	deps =
		[selections.ocaml]
		++ (map (name: builtins.getAttr name selections) names);
}

