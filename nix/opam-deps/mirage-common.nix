(import ./server.nix) ++ [
	"crunch"
	"fat-filesystem"
	"io-page"
	"mirage-console"
	"mirage-entropy"
	"cohttp-mirage"
	"mirage-logs"
	"mirage-dns"
	"tcpip"
]
