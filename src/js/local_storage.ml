open Js
module Json = Json_ext

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
  method key : int meth
end

let local_storage : local_storage t = Js.Unsafe.variable "localStorage"

class type record_i = object
  method refresh:unit
end

class ['s] record_t key =
  let key = Js.string key in
  let listeners = ref [] in
  object (self)
  method get_str = local_storage##getItem(key) |> Opt.to_option
  method get = self#get_str |> Option.map to_string |> Option.map Json.from_string

  method save ?(step:'s option) v =
    local_storage##setItem(key, Js.string (Json.to_string v));
    self#update ?step (Some v)

  method delete ?(step:'s option) =
    local_storage##removeItem(key);
    self#update ?step None

  method watch l = listeners := l :: !listeners
  method unwatch l =
    let expected = List.length !listeners in
    listeners := !listeners |> List.filter (fun item -> item == l);
    if List.length !listeners <> expected then raise Not_found

  method refresh = self#update (self#get)

  method private update ?(step:'s option) v = !listeners |> List.iter (fun l -> l ?step v)
end

let _records:record_i list ref = ref []

let erase_all () = local_storage##clear();
  !_records |> List.iter (fun r -> r#refresh)


let record key =
  let rv = new record_t key in
  _records := (rv:>record_i) :: !_records;
  rv
