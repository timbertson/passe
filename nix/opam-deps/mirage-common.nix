(import ./server.nix) ++ [
	"fat-filesystem"
	"io-page"
	"mirage-console"
	"mirage-dns"
	"mirage-http"
	{ "tcpip" = ">=2.6.1"; }
]
