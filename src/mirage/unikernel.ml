open Lwt
open V1_LWT
open Printf

module Main (C: CONSOLE) (CON:Conduit_mirage.S) (Fs:Passe_server.Filesystem.FS) (C:V1.CLOCK) = struct
  module Cohttp   = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module Cohttp_server = struct
    include Cohttp
    let respond_file ?headers ~fname () =
      ignore fname;
      Cohttp.respond_string
        ?headers
        (* ~headers:(json_content_type |> no_cache) *)
        ~status:`OK ~body:"TODO!" ()
    let resolve_file ~docroot ~uri = raise Not_found
  end

  module Logging = Passe.Logging.Make(Passe.Logging.Unix_output)
  module Fs = struct
    include Passe_server.Filesystem.Make(Fs)(Logging)
    let rename fs a b =
      (* XXX completely non-atomic, poorly-performant, and, may definitely fail halfway through.
       * TODO: before relying on this server, figure out a better way to do this *)
      lwt contents = read_file fs a in
      let open Lwt in
      let write_new : unit Lwt.t = (write_file fs b contents |> unwrap_lwt "write_file") in
      let destroy_old : unit Lwt.t = (destroy fs a |> unwrap_lwt "destroy") in
      write_new >> destroy_old
  end

  module Auth = Passe_server.Auth.Make(Logging)(C)(Passe_server.Hash.Hash_sha256)(Fs)
  module Server = Passe_server.Service.Make(Logging)(Cohttp_server)(Fs)(Auth)(Passe.Re_native)

  let start console conduit fs clock =
    let http_callback = Server.handler
      ~document_root:"TODO"
      ~data_root:(ref "TODO")
      ~user_db:(ref (new Auth.storage fs "/dev/null"))
      ~fs
      ~enable_rc:false
    in

    let spec = Cohttp.make ~callback:http_callback () in
    CON.listen conduit (`TCP 8082) (Cohttp.listen spec)
end

