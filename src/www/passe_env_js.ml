open Passe
module J = Json_ext
open Js
open Passe_env

let env = lazy (Unsafe.get (Unsafe.global) "PasseEnv")
let env_prop name : 'a Js.t = Unsafe.get (Lazy.force env) name
let as_bool x : bool Js.opt =
	  if typeof x |> to_string = "boolean"
		then some (Unsafe.coerce x |> to_bool)
		else null

let get_bool dfl name = Opt.get (env_prop name |> as_bool) (fun () ->
	prerr_endline ("Warning: could not coerce PasseEnv." ^ name ^ " to a boolean");
	dfl
)

let offline_access = lazy (get_bool true offline_access_key)
let auth_mode = lazy (
	if (get_bool false implicit_auth_key)
		then `Implicit
		else `Explicit
)

