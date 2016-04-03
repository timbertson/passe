(* all of the paramaterized modules for unix *)
open Passe
module Re = Re_native
module Logging = Logging.Make(Logging.Unix_output)
module Config = Config.Make(Logging)
module Date = Date_unix
module Server = Server_unix
module Sync = Sync.Make(Server)(Date)(Re)(Logging)
module Store = Store.Make(Re)(Logging)
module Version = Version.Make(Re)
module Domain = Domain.Make(Re)
module Password = Password.Make(Re)(Logging)
module Client_auth = Client_auth.Make(Server)(Logging)
