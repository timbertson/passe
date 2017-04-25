include Cstruct
let string_of_list : t list -> string = fun chunks ->
	List.map (Cstruct.to_string) chunks |> String.concat ""
