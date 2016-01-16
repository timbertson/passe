open Mirage

let handler = foreign "Unikernel.Main" (console @-> conduit @-> fs @-> clock @-> job)

let () =
  let console = default_console in

  let fs = match get_mode() with
    (* XXX fat_of_files doesn't seem to work on Xen, because actual xen attaches devices with an aio: prefix. So we fake it a bit: *)
    | `Xen -> fat (block_of_file "aio:fat1.img")
    | _ -> fat (block_of_file "_build/fat1.img")
  in
  let stack = match get_mode() with
    | `Xen -> direct_stackv4_with_dhcp console tap0
    | _ -> socket_stackv4 default_console [Ipaddr.V4.any]
  in
  let conduit = conduit_direct ~tls:false stack in
  add_to_ocamlfind_libraries ([
    (* XXX what's the logic of adding libraries here? I think it's only needed
     * when those libraries have stublibs or native c libs which are used during linking *)
    "nocrypto" ; (* triggers entropy initialization *)
    "safepass" ;
    "sha";
  ] @ (match get_mode() with
    | `Xen -> [ "zarith-xen" ]
    | _ -> []
  ));
  register "console" [
    handler $ console $ conduit $ fs $ default_clock
  ]
