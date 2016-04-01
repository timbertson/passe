open Passe
module J = Json_ext
open Js

let env = lazy (Unsafe.get (Unsafe.global) "PasseEnv")
let get dfl name = Lazy.force env |> J.get_field name |> Option.default dfl

let offline_access = lazy (get true Passe_env_keys.offline_access)
let implicit_auth = lazy (get false Passe_env_keys.implicit_auth)

