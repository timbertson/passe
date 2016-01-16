(import ./mirage-common.nix) ++ [
	"gmp-xen"
	"mirage-block-xen"
	"mirage-clock-xen"
	"mirage-net-xen"
	"mirage-xen"
	"zarith-xen"
]
# XXX these are to force mirage-console to have the `xen` backend, which is a bit awkward
++ [ "mirage-xen" "xenstore" "xen-gnt" "xen-evtchn" "shared-memory-ring" "xenstore" "xenstore_transport" ]
