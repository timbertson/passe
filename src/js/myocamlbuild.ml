open Ocamlbuild_plugin ;;
let () =
	flag ["ocaml";"compile";] (S [A"-ppopt"; A ("-DJS")]);
	flag ["ocaml";"ocamldep";] (S [A"-ppopt"; A ("-DJS")])
