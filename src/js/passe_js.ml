(* all of the paramaterized modules for JS *)
open Passe
module Re = Re_js
module Date = Date_js
module Server = Server_js
module Sync = Sync.Make(Server)(Date)(Re)
module Store = Store.Make(Re)
module Version = Version.Make(Re)
(* module Domain = Domain.Make(Re) *)
module Password = Password.Make(Re)
module Client_auth = Client_auth.Make(Server)
