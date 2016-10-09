open Passe
open Tyxml
module J = Json_ext

let string_of_html h = 
	let b = Buffer.create 255 in
	let fmt = Format.formatter_of_buffer b in
	Html.pp () fmt h;
	Format.pp_print_flush fmt ();
	Buffer.contents b

let safe_string_of_json o = o |> J.to_string |> Str.global_replace (Str.regexp "/") "\\u003c"

let html ~implicit_auth ~offline_access () =
	let open Html in
	let html_attrs = if offline_access
		then [ a_manifest "index.appcache" ]
		else []
	in

	let passe_env_json = `Assoc [
		Passe_env.offline_access_key, `Bool offline_access;
		Passe_env.implicit_auth_key, `Bool implicit_auth;
	] |> safe_string_of_json in

	let head = <:html<
		<head>
			<meta charset="utf-8"/>
			<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>
			<link rel="icon" href="/res/images/16.png"/>
			<link rel="icon" href="/res/images/32.png" sizes="32x32"/>
			<link rel="icon" href="/res/images/128.png" sizes="128x128"/>
			<link href="res/css/style.css" rel="stylesheet"/>
			<title>Passé</title>
		</head>
	>> in

	let body = <:html<
		<body>
			<div id="main">
				<div class="container main">
					<h4 class="text-center text-muted">Loading...</h4>
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
						node.appendChild(text);
						e.appendChild(node);
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

	Html.html ~a:([a_lang "en"] @ html_attrs) head body

