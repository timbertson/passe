open Passe
module Json_ext = Json_ext
module Common = Common
include Passe_unix
include OUnit2
module J = Json_ext

let identity: 'a -> 'a = fun x -> x


(* common assertion modules *)
module type ASSERT_TYPE = sig
	type t
	val printer : t -> string
	val cmp : (t -> t -> bool) option
end

module MakeAssert(Type:ASSERT_TYPE) = struct
	include Type
	let cmp = match cmp with | Some c -> c | None -> (=)
	module L = struct
		let printer ?(brackets=("[","]")) lst =
			let (open_b, close_b) = brackets in
			open_b ^ (List.map Type.printer lst |> String.concat ", ") ^ close_b

		let cmp a b =
			if List.length a = List.length b then (
				List.for_all2 cmp a b
			) else false

		let assert_equal = assert_equal ~cmp ~printer
	end

	let assert_equal = assert_equal ~printer ~cmp
end

module S = MakeAssert(struct
	type t = string
	let cmp = None
	let printer = identity
end)

module I = MakeAssert(struct
	type t = int
	let cmp = None
	let printer = string_of_int
end)

module B = MakeAssert(struct
	type t = bool
	let cmp = None
	let printer = string_of_bool
end)

(* module-wide setup *)
let (>::) desc test =
	let initial_reporter = Logs.reporter () in
	let setup ctx =
		let ppf, flush =
			let b = Buffer.create 255 in
			let flush () = let s = Buffer.contents b in Buffer.clear b; s in
			Format.formatter_of_buffer b, flush
		in

		let report_fn _src level ~over k msgf =
			let l = match level with
				| Logs.Error -> `Error
				| Logs.Warning -> `Warning
				| Logs.App | Logs.Info | Logs.Debug -> `Info
			in
			let k _ =
				logf ctx l "%s" (flush ());
				over ();
				k ()
			in
			msgf @@ fun ?header:(_) ?tags:(_) fmt ->
			Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
		in
		Logs.(set_reporter { report = report_fn })


	in
	let teardown () _ =
		Logs.set_reporter initial_reporter
	in
	desc >:: (fun ctx ->
		bracket setup teardown ctx;
		test ctx
	)
