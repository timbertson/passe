open Passe

type datastore = {
	url : string;
}

module Impl : Dynamic_store.Concrete = struct
	include Dynamic_store.Core
	type t = datastore

	let connect_str url = { url }
	let connect_unix_fs _ _ = Error.raise_assert "Not implemented"

	let read = Obj.magic
	let write = Obj.magic

	let read_s = Obj.magic
	let read_for_writing = Obj.magic
	let write_s = Obj.magic

	let delete = Obj.magic
	let reconnect = Obj.magic
end

include Impl
