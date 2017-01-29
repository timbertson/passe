module Make (Re:Re_ext.Sig) = struct
	type t = string
	let raw s = s

	let hostname s =
		let uri = Uri.of_string s in
		match Uri.host uri with
			| Some host -> host
			| None -> s |> Re.replace_first (Re.regexp "/.*") ""

	let known_common_slds () = Domain_list.slds

	let re_dot = Re.regexp "\\."

	let guess s =
		let s = hostname s in
		let parts = Re.split (re_dot) s in
		let part = List.nth parts in
		let len = List.length parts in
		let guess = (
			if len <= 2
			then s
			else (
				let known_common_slds = known_common_slds () in
				(* # Take the top-level and second-level domain *)
				let domain = part (len - 2) ^ "." ^ part (len - 1) in
				(* # If this is one of the known second-level domains, take the third *)
				(* # level as well *)
				if List.mem domain known_common_slds
					then part (len - 3) ^ "." ^ domain
					else domain
			)
		) in
		Option.non_empty ~zero:"" guess

end
