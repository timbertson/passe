open Js

class type document = object
  method execCommand : string -> bool -> _ -> bool meth
end

let document : document t = Js.Unsafe.get (Js.Unsafe.global) "document"
let triggerCopy () =
  if document##execCommand "copy" true ()
  then None
  else Some "Copy command not supported"
