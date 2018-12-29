type datastore = {
	url : string;
}

module Impl : Kv_store.Sig = struct
	include Kv_store.Core
	type t = unit

	let read = Obj.magic
	let write = Obj.magic

	let read_s = Obj.magic
	let read_for_writing = Obj.magic
	let write_s = Obj.magic

	let delete = Obj.magic
	let connect = Obj.magic
end

include Impl
