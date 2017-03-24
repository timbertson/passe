open Mirage

let main = impl @@ object
  inherit [_] foreign ~packages:[] "Unikernel.Main" (conduit @-> fs @-> pclock @-> job)
  method packages =
    let open Functoria in
    let base = [
      package "nocrypto" ; (* triggers entropy initialization *)
      package "safepass" ;
      package "sha";
    ] in
    Key.(if_ is_unix) base (base @ [package "zarith-xen"])
end

let () =
  let console = default_console in

  let fs = fat @@ block_of_file "fat1.img" in
  let stack = generic_stackv4 default_network in
  let conduit = conduit_direct ~tls:false stack in
  let target = Mirage_key.target in
  register "main" [
    main $ conduit $ fs $ default_posix_clock
  ]
