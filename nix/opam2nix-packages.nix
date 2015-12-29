{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	dev_repo = builtins.getEnv "OPAM2NIX_DEVEL";
	toPath = s: /. + s;
	in if dev_repo != ""
		then callPackage "${dev_repo}/nix" {} {
				src = toPath "${dev_repo}/nix/local.tgz";
				opam2nix = toPath "${dev_repo}/opam2nix/nix/local.tgz";
			}
		else let
			src = fetchgit {
				fetchSubmodules = false;
				url = "https://github.com/timbertson/opam2nix-packages.git";
				rev = "40c1c73a0ec150e4b4cab03e1fa773e50230af5e";
				sha256 = "ee2ade1eb70c378b449bea9f03896327408da93a9ecb290638fd87e7ea441fa6";
			};

			# We could leave this out and just use `fetchSubmodules` above,
			# but that leads to mass-rebuilds every time the repo changes
			# (rather than only when opam2nix is updated)
			opam2nix = fetchgit {
				url = "https://github.com/timbertson/opam2nix.git";
				rev = "62fa8aa46aa0a9193e9e7dda92799237bde946bc";
				sha256 = "4096a0b2c02a6ec6209fea8cea1782b091c7fdd543d0975defeee4514961ce62";
			};
		in callPackage "${src}/nix" {} { inherit src opam2nix; }


