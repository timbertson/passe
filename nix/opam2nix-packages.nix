
{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	# For development, set OPAM2NIX_DEVEL to your local
	# opam2nix repo path
	devRepo = builtins.getEnv "OPAM2NIX_DEVEL";
	src = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix-packages.git";
		"fetchSubmodules" = false;
		"sha256" = "1b1mahgxyxba9bcgnz1vf31irzg8drm2la1b46himxqb4ghvqhjz";
		"rev" = "deaaebd12b3dced2eb16ef0c16e46c044a5340bb";
	};
	opam2nix = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix.git";
		"fetchSubmodules" = false;
		"sha256" = "00j2x5fgnhll12vkg9kygmnwh7y7b1dp5k8j2r46iq3zikgsv7q3";
		"rev" = "ac4212702eac44c3f15714d550f5d36487104a5a";
	};
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {} {
		src = toPath "${devRepo}/nix/local.tgz";
		opam2nix = let devSrc = "${devRepo}/opam2nix/nix/local.tgz"; in
			if builtins.pathExists devSrc then toPath devSrc else opam2nix;
	}
else callPackage "${src}/nix" {} { inherit src opam2nix; }
