module Main (CON:Conduit_mirage.S) (Static_kv:Passe_server.Static.Kv_RO) (Fs:Passe_server.Filesystem.FS) (Clock:Mirage_types.PCLOCK) = struct
  module Cohttp = Cohttp_mirage.Server(Conduit_mirage.Flow)
  module PasseFS = Passe_server.Filesystem.Make(Fs)(Passe_server.Filesystem_xen.Atomic)
  module Cohttp_server = Cohttp
  module Auth = Passe_server.Auth.Make(Clock)(Passe_server.Hash_bcrypt)(PasseFS)
  module Static_res = Passe_server.Static.Readonly(Static_kv)
  module Service = Passe_server.Service.Make(Clock)(Static_res)(PasseFS)(Cohttp_server)(Passe_server.Server_config_mirage)(Auth)(Passe.Re_native)
  module Timed_log = Passe_server.Timed_log.Make(Clock)

  let start conduit static fs clock =
    let basic_reporter = Logs.reporter () in
    Passe.Logging.set_reporter (Timed_log.reporter ~clock basic_reporter);
    Logs.(set_level ~all:true (Some Debug));
    let data_root = "/data" in
    let http_callback = Service.handler
      ~static:(Static_res.init static)
      ~data_root:data_root
      ~clock
      ~fs
      ~development:false
      ~enable_rc:false
    in

    let spec = Cohttp.make ~callback:http_callback () in
    CON.listen conduit (`TCP 8082) (Cohttp.listen spec)
end

