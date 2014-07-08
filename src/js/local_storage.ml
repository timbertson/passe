open Js
open React_ext
module Json = Json_ext

(* interface Storage {
  readonly attribute unsigned long length;
  [IndexGetter] DOMString key(in unsigned long index);
  [NameGetter] DOMString getItem(in DOMString key);
  [NameSetter] void setItem(in DOMString key, in DOMString data);
  [NameDeleter] void removeItem(in DOMString key);
  void clear();
}; *)

module StringMap = Map.Make(String)

class virtual provider =
  let cache: record StringMap.t ref = ref StringMap.empty in
  object (self)
  method create key =
    try
      StringMap.find key !cache
    with Not_found -> (
      let rv = new record (self:>provider) key in
      cache := StringMap.add key rv !cache;
      rv
    )

  method erase_all =
    self#_erase_all;
    StringMap.bindings !cache |> List.iter (fun (_k,v) -> v#refresh)

  method virtual get : string -> string option
  method virtual set : string -> string -> unit
  method virtual delete : string -> unit
  method virtual _erase_all : unit
end

and record (impl:provider) key =
  let get () = impl#get key |> Option.map Json.from_string in
  let signal, update_signal = S.create (get ()) in
  object (self)
  method get = get ()

  method save ?(step:step option) v =
    impl#set key (Json.to_string v);
    self#update ?step (Some v)

  method delete ?(step:step option) =
    impl#delete key;
    self#update ?step None

  method refresh = self#update (self#get)

  method signal = signal

  method private update ?(step:step option) v =
    update_signal ?step v
end

class in_memory_provider =
  let root = ref StringMap.empty in
  object (self)
  inherit provider
  method get key = try Some(StringMap.find key !root) with Not_found -> None
  method set key value = root := StringMap.add key value !root
  method delete key = root := StringMap.remove key !root
  method _erase_all = root := StringMap.empty
end

class type local_storage = object
  method getItem : js_string t -> js_string t opt meth
  method setItem : js_string t -> js_string t -> unit meth
  method removeItem : js_string t -> unit meth
  method clear : unit meth
  method key : int meth
end

class browser_provider =
  let local_storage : local_storage t = Js.Unsafe.variable "localStorage" in
  object (self)
  inherit provider
  method get key = local_storage##getItem(Js.string key) |> Opt.to_option |> Option.map to_string
  method set key value = local_storage##setItem(Js.string key, Js.string value)
  method delete key = local_storage##removeItem(Js.string key)
  method _erase_all = local_storage##clear()
end

let persistent = lazy (new browser_provider)
let ephemeral = lazy (new in_memory_provider)
let erase_all impl = impl#erase_all
let record ~(impl:provider) key : record = impl#create key
