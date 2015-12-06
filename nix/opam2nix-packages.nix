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
				rev = "fbeef9f5ea0b21c6f6c09f8462b7369f50e9886d";
				sha256 = "4b4b3e90fbb80db9c0cc847f8a168fbf633a1bc76a1d6383579f620540b57b94";
			};

			# We could leave this out and just use `fetchSubmodules` above,
			# but that leads to mass-rebuilds every time the repo changes
			# (rather than only when opam2nix is updated)
			opam2nix = fetchgit {
				url = "https://github.com/timbertson/opam2nix.git";
				rev = "e8f697be173ce1205b80786bd7a9d9c64464ccc1";
				sha256 = "2216aa9f692ca875b449c1f051f2b88bde502ed000ffd7efae0fe7085f66e7a1";
			};
		in callPackage "${src}/nix" {} { inherit src opam2nix; }


