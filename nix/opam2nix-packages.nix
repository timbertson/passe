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
				rev = "50042127608ad511d4d456f07087515e4e2dceca";
				sha256 = "73912a6ec86bdb5ed1fa2c294f7a732d60c729f578bbadc43da4f97addf13f42";
			};

			# We could leave this out and just use `fetchSubmodules` above,
			# but that leads to mass-rebuilds every time the repo changes
			# (rather than only when opam2nix is updated)
			opam2nix = fetchgit {
				url = "https://github.com/timbertson/opam2nix.git";
				rev = "372998a1624a6de9922fe924960a6e19281526af";
				sha256 = "49de441064aef7a1755a8d27e7f097e02df667a5fa2611051625972c5b54df08";
			};
		in callPackage "${src}/nix" {} { inherit src opam2nix; }


