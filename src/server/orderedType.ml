module type S = sig
	include Map.OrderedType
	val pp : t Fmt.t
end
