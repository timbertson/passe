type datastore = {
	url : string;
}

module Impl = struct
	include Dynamic_store.Core
	type t = datastore

	let connect url = { url }

	let read = Obj.magic
	let write = Obj.magic

	let read_s = Obj.magic
	let read_for_writing = Obj.magic
	let write_s = Obj.magic

	let delete = Obj.magic
	let reconnect = Obj.magic
end

include Impl
