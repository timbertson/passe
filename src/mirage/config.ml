open Mirage

let main = impl @@ object
  inherit [_] foreign ~packages:[] "Unikernel.Main" (conduit @-> kv_ro @-> fs @-> pclock @-> job)
  method packages =
    let open Functoria in
    let base = [
      package "nocrypto" ; (* triggers entropy initialization *)
      package "safepass" ;
    ] in
    Key.(if_ is_unix) base (base @ [package "zarith-freestanding"])
end

let () =
  let console = default_console in
  let build_root = "../../_build/" in
  let fat_name = "fat1.img" in
  let docroot_path = build_root ^ "docroot" in
  let fat_img_path = build_root ^ fat_name in
  assert ((Sys.command ("gup -u " ^ docroot_path ^ " " ^ fat_img_path)) == 0);

  let static = crunch docroot_path in
  let fs = fat @@ block_of_file "fat1.img" in
  let stack = generic_stackv4 default_network in
  let conduit = conduit_direct ~tls:false stack in
  let target = Mirage_key.target in
  register "main" [
    main $ conduit $ static $ fs $ default_posix_clock
  ]
