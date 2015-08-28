(* all of the paramaterized modules for Xen *)
open Passe
module Re = Re_native
module Date = Clock_xen
module Server = Server_js
module Store = Store.Make(Re)
module Version = Version.Make(Re)
module Domain = Domain.Make(Re)
module Password = Password.Make(Re)

