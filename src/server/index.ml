open Passe
module Str = Re_str
module J = Json_ext

type html = string
let string_of_html : html -> string = fun x -> x

let safe_string_of_json o = o |> J.to_string |> Str.global_replace (Str.regexp "/") "\\u003c"

let html ~implicit_auth ~offline_access () : html =
	let passe_env_json = `Assoc [
		Passe_env.offline_access_key, `Bool offline_access;
		Passe_env.implicit_auth_key, `Bool implicit_auth;
	] |> safe_string_of_json in

	let head = ("\
		<head>\
			<meta charset=\"utf-8\"/>\
			<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no\"/>\
			<link rel=\"icon\" href=\"/res/images/16.png\"/>\
			<link rel=\"icon\" href=\"/res/images/32.png\" sizes=\"32x32\"/>\
			<link rel=\"icon\" href=\"/res/images/128.png\" sizes=\"128x128\"/>\
			<link href=\"style.css\" rel=\"stylesheet\"/>\
			<title>Passé</title>\
		</head>\
	") in

	let script = ("\
		window.PasseEnv = " ^ passe_env_json  ^ ";\n\
		window.onerror = function(error) {\n\
			console.error(error);\n\
			var elem = document.getElementById('main');\n\
			elem.innerHTML = '';\n\
\n\
			var outer = document.createElement('div');\n\
			outer.setAttribute('class', 'uncaught-error');\n
			elem.appendChild(outer);\n\
			var tag = function(name, text) {\n\
				var node=document.createElement(name);\n\
				node.appendChild(document.createTextNode(text));\n\
				outer.appendChild(node);\n\
			};\n\
\n\
			tag('h1', 'Uncaught Error');\n\
			tag('p', 'Sorry, an uncaught error occurred:');\n\
			tag('p', String(error));\n\
			tag('p', 'Please reload the page to try again');\n\
		}\
	") in

	let body = ("\
		<body>\
			<div id=\"main\">\
				<div class=\"container main\">\
					<h4 class=\"text-center text-muted\">Loading...</h4>\
				</div>\
			</div>\
			<script>" ^ script ^ "</script>\
			<script src=\"main.bc.js\"></script>\
		</body>\
	") in

	"<html lang=\"en\">" ^ head ^ body ^ "</html>"

