open Js
open React_ext
module Json = Json_ext
let log = Logging.get_logger "local_storage"

module StringMap = Map.Make(String)

class type base = object
  method get : string -> string option
  method set : string -> string -> unit
  method delete : string -> unit
  method iter : (string -> string -> unit) -> unit
  method erase_all : unit
end

class in_memory_provider : base =
  let root = ref StringMap.empty in
  object (self)
  method get key = try Some(StringMap.find key !root) with Not_found -> None
  method set key value = root := StringMap.add key value !root
  method delete key = root := StringMap.remove key !root
  method iter fn = StringMap.iter fn !root
  method erase_all =
    log#debug "erasing in-memory storage provider";
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

class browser_provider : base =
  let local_storage : local_storage t = Js.Unsafe.variable "localStorage" in
  object (self)
  method get key = local_storage##getItem(Js.string key) |> Opt.to_option |> Option.map to_string
  method set key value = local_storage##setItem(Js.string key, Js.string value)
  method delete key = local_storage##removeItem(Js.string key)
  method iter (fn : string -> string -> unit) =
    let len = local_storage##length in
    let i = ref 0 in
    let force opt = Opt.get opt (fun () -> assert false) in
    while !i < len do
      let key = local_storage##key(!i) |> force in
      let value = local_storage##getItem(key) |> Opt.to_option in
      value |> Option.may (fun value -> fn (to_string key) (to_string value));
      i := succ !i;
    done

  method erase_all:unit =
    log#debug "erasing localStorage";
    local_storage##clear()
end


class record (impl:provider) key =
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

and provider do_persist =
  let persistent = new browser_provider in
  let ephemeral = new in_memory_provider in
  let active: base ref = ref (if do_persist then persistent else ephemeral) in
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
    persistent#erase_all;
    ephemeral#erase_all;
    StringMap.bindings !cache |> List.iter (fun (_k,v) -> v#refresh)

  method get key = !active#get key
  method set key value = !active#set key value
  method delete key = !active#delete key
  method set_persistent p =
    log#info "setting persistent to: %b" p;
    let new_impl = if p then persistent else ephemeral in
    if new_impl != !active then (
      new_impl#erase_all;
      log#debug "transferring data";
      !active#iter new_impl#set;
      !active#erase_all;
      active := new_impl;
    )
end

let erase_all impl = impl#erase_all
let record ~(impl:provider) key : record = impl#create key
