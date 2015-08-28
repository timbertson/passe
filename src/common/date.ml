module type Sig = sig
	val time : unit -> float
	val human_time_span_desc : float -> string
end
