(import ./common.nix) ++ (import ./unix-common.nix) ++ [
	"conduit"
	{ name = "js_of_ocaml"; constraint = ">=3.2.0"; }
	"re"
	"safepass"
	"tcpip"
	"vdoml"
]
