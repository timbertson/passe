[
	"astring"
	"cohttp"

	# I don't care about base64, but `cohttp` uses it so we need to carefully select
	# a version which doesn't clash with `extlib` (at least 2.x, but before 3.x)
	# See https://github.com/mirage/ocaml-base64/pull/25
	# TODO: should be >=2.0.0 as well, but opam2nix doesn't support that yet
	# { name = "base64"; constraint = "<=3.0.0 & >=2.0.0"; } (TODO: support multiple constraints in opam2nix)
	{ name = "base64"; constraint = "<=3.0.0"; }
	"hex"
	"dune"
	"logs"
	{ name = "lwt"; constraint = ">=3.1.0"; }
	"lwt_react"
	"lwt_ppx"
	"mirage"
	"mirage-clock-unix"
	"mirage-fs-unix"
	"mirage-types"
	"mirage-types-lwt"
	"ocamlfind"
	"ocamlbuild"
	"opam-installer"
	"ounit"
	"ptime"
	"react"
	"rresult"
	{ name = "uri"; constraint = ">=1.9.4"; }
	{ name = "yojson"; constraint = "<1.6.0"; }
]
