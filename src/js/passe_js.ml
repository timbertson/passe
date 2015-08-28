(* all of the paramaterized modules for JS *)
open Passe
module Logging = Logging.Make(Logging.Js_output)
module Config = Config.Make(Logging)
module Re = Re_js
module Date = Date_js
module Server = Server_js
module Sync = Sync.Make(Server)(Date)(Re)(Logging)
module Store = Store.Make(Re)(Logging)
module Version = Version.Make(Re)
(* module Domain = Domain.Make(Re) *)
module Password = Password.Make(Re)(Logging)
module Client_auth = Client_auth.Make(Server)
