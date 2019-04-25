(import ./common.nix) ++ (import ./unix-common.nix) ++ [
	"conduit"
	"dns-lwt-unix"
	{ name = "js_of_ocaml"; constraint = ">=3.2.0"; }
	{ name = "nocrypto"; constraint = ">=0.5.4"; }
	"re"
	"safepass"
	"tcpip"
	"vdoml"
]
