(* all of the paramaterized modules for unix *)
open Passe
module Re = Re_native
module Date = Date_unix
module Server = Server_unix
module Sync = Sync.Make(Server)(Date)(Re)
module Store = Store.Make(Re)
module Version = Version.Make(Re)
module Domain = Domain.Make(Re)
module Password = Password.Make(Re)
module Client_auth = Client_auth.Make(Server)
