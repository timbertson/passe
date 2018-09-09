(import ./common.nix) ++ (import ./unix-common.nix) ++ [
	"astring"
	"conduit"
	{ name = "js_of_ocaml"; constraint = ">=3.2.0"; }
	"re"
	"safepass"
	"tcpip"
	"vdoml"
]
