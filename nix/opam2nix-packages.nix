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
				rev = "ee0026f617a7e659a676cfd307c08c0196c3dd54";
				sha256 = "f29341ff1490f125d557fcaf2bf60ad6f4253538636ae31b1a97a3943a436f43";
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


