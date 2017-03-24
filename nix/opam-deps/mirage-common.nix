(import ./server.nix) ++ [
	"fat-filesystem"
	"io-page"
	"mirage-console"
	"mirage-http"
	"mirage-logs"
	"mirage-dns"
	"tcpip"
]
