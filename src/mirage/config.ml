open Mirage

let handler = foreign "Unikernel.Main" (console @-> conduit @-> fs @-> clock @-> job)

(* let fs = fat_of_files ~dir:"./data" () *)
(* XXX fat_of_files doesn't seem to work, because actual xen attaches devices with an aio: prefix. So we fake it a bit: *)
let fs = fat (block_of_file "aio:fat1.img")


let () =
  let console = default_console in
  let stack = match get_mode() with
    (* | `Xen -> socket_stackv4 default_console [Ipaddr.V4.any] *)
    | `Xen -> direct_stackv4_with_dhcp console tap0
    | _ -> direct_stackv4_with_default_ipv4 console tap0
  in
  let conduit = conduit_direct ~tls:false stack in
  register "console" [
    handler $ console $ conduit $ fs $ default_clock
  ]
