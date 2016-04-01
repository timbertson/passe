open Passe
module J = Json_ext

let string_of_html h = 
	let b = Buffer.create 16 in
	Html5.P.print ~output:(Buffer.add_string b) h;
	Buffer.contents b

let safe_string_of_json o = o |> J.to_string |> Str.global_replace (Str.regexp "/") "\\u003c"

let html ~implicit_auth ~offline_access () =
	let open Html5.M in
	let module Html5 = Html5.M in

	let html_attrs = if offline_access
		then [ a_manifest "index.appcache" ]
		else []
	in

	let passe_env_json = `Assoc [
		Passe_env_keys.offline_access, `Bool offline_access;
		Passe_env_keys.implicit_auth, `Bool implicit_auth;
	] |> safe_string_of_json in

	let head = <:html5<
		<head>
			<meta charset="utf-8"/>
			<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>
			<link href="res/css/style.css" rel="stylesheet"/>
			<title>Passé</title>
		</head>
	>> in

	let body = <:html5<
		<body>
			<div id="main">
				<div class="container">
					<h1>Loading...</h1>
				</div>
			</div>
			<script>
				window.PasseEnv = $str:passe_env_json$;
				window.onerror = function(e) {
					console.error(e);
					var e = document.getElementById("main");
					e.innerHTML = "";

					var tag = function(name, text) {
						var node=document.createElement(name);
						node.appendContent(text);
						e.appendNode(node);
					}

					tag("h1", "Uncaught Error");
					tag("p", "Sorry, an uncaught error occurred:");
					tag("p", String(e));
					tag("p", "Pleare reload the page to try again");
				}
			</script>
			<script src="js/main.js"></script>
		</body>
	>> in

	Html5.html ~a:([a_lang "en"] @ html_attrs) head body
	(* << <html lang="en"> *)
	(* 	</head> *)
	(* </html> *)
	(* </html> >> *)


