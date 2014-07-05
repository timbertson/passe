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

let erase_all () = local_storage##clear()

class record key =
  let key = Js.string key in
  let listeners: (Json.json -> unit) list ref = ref [] in
  object (self)
  method get_str = local_storage##getItem(key) |> Opt.to_option
  method get = self#get_str |> Option.map to_string |> Option.map Json.from_string

  method save v =
    local_storage##setItem(key, Js.string (Json.to_string v));
    self#update v

  method delete =
    local_storage##removeItem(key);
    self#update `Null

  method watch l = listeners := l :: !listeners
  method unwatch l =
    let expected = List.length !listeners in
    listeners := !listeners |> List.filter (fun item -> item == l);
    if List.length !listeners <> expected then raise Not_found

  method private update v = !listeners |> List.iter (fun l -> l v)
end
