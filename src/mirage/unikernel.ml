open Lwt
open V1_LWT
open Printf

module Main (C: CONSOLE) (CON:Conduit_mirage.S) (Fs:Passe_server.Filesystem.FS) (C:V1.CLOCK) = struct
  module Cohttp   = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module PasseFS = Passe_server.Filesystem.Make(Fs)(Passe_server.Filesystem_xen.Atomic)
  module Cohttp_server = Cohttp
  module Auth = Passe_server.Auth.Make(C)(Passe_server.Hash_bcrypt)(PasseFS)
  module Server = Passe_server.Service.Make(PasseFS)(Cohttp_server)(Auth)(Passe.Re_native)
  module Timed_log = Passe_server.Timed_log.Make(Clock)

  let start console conduit fs clock =
    Logging.set_reporter (Timed_log.reporter (Logs.reporter ()));
    Logs.(set_level ~all:true (Some Debug));
    let data_root = "/data" in
    let http_callback = Server.handler
      ~document_root:"/www"
      ~data_root:(ref data_root)
      ~user_db:(ref (Server.make_db fs data_root))
      ~fs
      ~development:false
      ~enable_rc:false
    in

    let spec = Cohttp.make ~callback:http_callback () in
    CON.listen conduit (`TCP 8082) (Cohttp.listen spec)
end

