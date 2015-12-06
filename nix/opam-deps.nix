{target, pkgs ? null}:
with pkgs;
let
	opam2nix-packages = import ./opam2nix-packages.nix { inherit pkgs; };
	names = import "${./opam-deps}/${target}.nix";
	selections = assert pkgs != null;
		let
			selections_file = opam2nix-packages.select {packages = names;};
		in
		opam2nix-packages.import selections_file {
			overrides = {super, self}: if target == "mirage-xen" then
			let
				mirage-platform-src = fetchgit {
					url = "https://github.com/timbertson/mirage-platform";
					rev = "623d1184278e929e35efad2b4ab86fccceae20de";
					sha256 = "d2a5427d2b9aeef236c1e4ce9ae0275c8e9db878ca202a93b8c4dd96598b3e50";
				};
				sels = super.opamSelection;
			in
			{
				opamSelection = super.opamSelection // {
					mirage-xen-posix = lib.overrideDerivation sels.mirage-xen-posix (o: {
						src = mirage-platform-src;
					});
					mirage-xen-ocaml = lib.overrideDerivation sels.mirage-xen-ocaml (o: {
						src = mirage-platform-src;
					});
					mirage-xen = lib.overrideDerivation sels.mirage-xen (o: {
						src = mirage-platform-src;
					});
					io-page = lib.overrideDerivation sels.io-page (o: {
						src = fetchgit {
							url = "https://github.com/gfxmonk/io-page";
							rev = "6c4d5634ca1f8e8df0de9ec50b366ec7bd74984e";
							sha256 = "94ec05ffd213733cbe7c92291d06ea30122c4d9fa0c1bf74a912306d22cd0526";
						};
					});
					tcpip = lib.overrideDerivation sels.tcpip (o: {
						src = fetchgit {
							url = "https://github.com/gfxmonk/mirage-tcpip";
							rev = "c386b6a4a198cb122dd0b1fcb00291a09c6b1e98";
							sha256 = "0c70a6cf658eac0248cb345934384dc51149d0893ddbb00915968deced1c6ea0";
						};
					});
				};
			} else {};
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

