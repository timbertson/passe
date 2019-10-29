open Js_of_ocaml
open Passe
open Js
open Common
module Json = Json_ext
module Log = (val Logging.log_module "local_storage")

module StringMap = Map.Make(String)

class type base = object
  method get : string -> Json.json option
  method set : string -> Json.json -> unit
  method delete : string -> unit
  method iter : (string -> Json.json -> unit) -> unit
  method erase_all : unit
end

class in_memory_provider : base =
  let root = ref StringMap.empty in
  object (_self)
  method get key = try Some(StringMap.find key !root) with Not_found -> None
  method set key value = root := StringMap.add key value !root
  method delete key = root := StringMap.remove key !root
  method iter fn = StringMap.iter fn !root
  method erase_all =
    Log.debug (fun m->m "erasing in-memory storage provider");
    root := StringMap.empty
end

(* interface Storage {
  readonly attribute unsigned long length;
  [IndexGetter] DOMString key(in unsigned long index);
  [NameGetter] DOMString getItem(in DOMString key);
  [NameSetter] void setItem(in DOMString key, in DOMString data);
  [NameDeleter] void removeItem(in DOMString key);
  void clear();
}; *)

class type local_storage = object
  method getItem : js_string t -> js_string t opt meth
  method setItem : js_string t -> js_string t -> unit meth
  method removeItem : js_string t -> unit meth
  method clear : unit meth
  method key : int -> js_string t opt meth
  method length : int prop
end

class browser_provider =
  let local_storage : local_storage t = Js.Unsafe.variable "localStorage" in
  object (self : #base)
  method _get key =
    local_storage##getItem(key)
    |> Opt.to_option
    |> Option.map (Json.from_string % to_string)

  method get key = self#_get (Js.string key)

  method set key value = local_storage##setItem (Js.string key) (Js.string (Json.to_string value))
  method delete key = local_storage##removeItem (Js.string key)
  method iter (fn : string -> Json.json -> unit) =
    let len = local_storage##.length in
    let i = ref 0 in
    let force opt = Opt.get opt (fun () -> assert false) in
    while !i < len do
      let key = local_storage##key(!i) |> force in
      let value = self#_get key in
      value |> Option.may (fun value -> fn (to_string key) value);
      i := succ !i;
    done

  method erase_all:unit =
    Log.debug (fun m->m "erasing localStorage");
    local_storage##clear
end

and provider do_persist =
  let persistent = ((new browser_provider):>base) in
  let ephemeral = ((new in_memory_provider):>base) in
  let active: base ref = ref (if do_persist then persistent else ephemeral) in
  object (_self : #Config.provider_t)
  inherit Config.base_provider

  method _erase_all =
    persistent#erase_all;
    ephemeral#erase_all;

  method get key = !active#get key
  method set key value = !active#set key value
  method delete key = !active#delete key
  method set_persistent p =
    Log.info (fun m->m "setting persistent to: %b" p);
    let new_impl = if p then persistent else ephemeral in
    if new_impl != !active then (
      new_impl#erase_all;
      Log.debug (fun m->m "transferring data");
      !active#iter new_impl#set;
      !active#erase_all;
      active := new_impl;
    )
end

let erase_all impl = impl#erase_all
let record ~(impl:provider) key : Config.record = impl#create key
