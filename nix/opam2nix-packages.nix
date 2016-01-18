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
				rev = "91c344955c4bc2a1151d9fa246a2fb9eadfcbc76";
				sha256 = "511edd7a0a6cb80d5a16fd8b4e84709e5df7e8507d16b1c9e6d544ff867075bc";
			};

			# We could leave this out and just use `fetchSubmodules` above,
			# but that leads to mass-rebuilds every time the repo changes
			# (rather than only when opam2nix is updated)
			opam2nix = fetchgit {
				url = "https://github.com/timbertson/opam2nix.git";
				rev = "7f1a9e96f00d1ed82d054ad0d0fa419b4833c74c";
				sha256 = "5a72681210d4abae876876ad42b9e187fb0aa7044428ac893d69cad8429ea02d";
			};
		in callPackage "${src}/nix" {} { inherit src opam2nix; }


