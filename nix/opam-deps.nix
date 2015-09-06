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
			overrideSelections = sels: if target == "mirage-xen" then
			let mirage-platform-src = fetchgit {
				url = "https://github.com/gfxmonk/mirage-platform";
				rev = "6121921ef6e666f021a61f3570840108927f90d8";
				sha256 = "c22e03c43c823d057d240f61487e4b3659c7ec19b065b8acd14c2244d0ca6118";
			};
			in
			sels // {
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
						rev = "ce3dc3766626556b62ca0a10c3e6b85b338bff46";
						sha256 = "a8fe256ec84e3f07e8aa1bc0b743cfa4a71fbe8f2aeb263ebba879a109f88d54";
					};
				});
				tcpip = lib.overrideDerivation sels.tcpip (o: {
					src = fetchgit {
						url = "https://github.com/gfxmonk/mirage-tcpip";
						rev = "27bde287a8eaac87fc3016dc983c98605d3cdd54";
						sha256 = "e23b3b0428baaa348ff7adfab2473a6273c1847052a3c05c6db16a9a9fc4b6bd";
					};
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

