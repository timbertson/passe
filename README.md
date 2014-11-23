# Passé

There is a public instance running at [http://passe-gfxmonk.rhcloud.com/](),
but I make no guarantees about its uptime / stability.
If you prefer, you can [run your own instance](#run-own).

----

<!-- about -->

Passé is a [SuperGenPass](http://www.supergenpass.com/) compatible password tool.
It uses one-way cryptographic hashing to generate a unique (but repeatable)
password for every site you visit online,
while you only need to remember one "master" password.

Unlike most other password solutions, SuperGenPass is __completely stateless__. That means:

 - Your passwords are never stored anywhere - there is no secret database to steal!
 - It's 100% portable - you can use SuperGenPass anywhere, you don't need to bring anything with you.

### So why not just use SuperGenPass?

In addition to the core SuperGenPass functionality, Passé allows you to
store a central, non-sensitive database of the domains you visit
and the particular settings associated with each one. This is strictly a
convenience; you can still use any other SuperGenPass implementation in
a pinch to generate a compatible password (since they all use the same algorithm).

Having said that, it's a very _nice_ convenience. It means you can change certain settings and store hints per-domain.
Like perhaps which master password a given site uses, if you have multiple or are in the process of changing your master password.
It also helps you out when a site gets hacked - you can store a non-sensitive "suffix" which alters your
generated password without having to chang your master password."

### But you're storing my data online! Is that safe?

If your stored data were to be hacked, the attacker would have access to:

 - the list of your saved domains
 - the length, suffix and hints associated with each domain

That would be bad, but _your master password_ and
_any individual site's generated password_ are never sent to the server, and are never stored anywhere.
If you're not happy with that, you should just use SuperGenPass. Personally, I'm willing
to trade that minor risk for the convenience offered by Passé.

### Is my Passé database encrypted?

Nope. If you use the command-line tool, it'll be stored as a plain file.
If you use your browser, it's saved in local storage (unless you use the "incognito" checkbox).
You should make your hints as obtuse as possible; assume they could someday be read by someone else.

### Do I need to be online?

Nope. It uses the HTML5 Application Cache, and stores your DB locally so you can do
anything while offline, and it'll sync any DB changes when you're next online.

### What's the "suffix" field for?

Sites get hacked with alarming frequency. If your generated password is compromised you may want to add (or change) a suffix for the affected domain.
Passé will append the stored suffix to your master password when generating the site-specific password, giving you a completely
new site password without having to change your master password.

Of course, if you suspect your master password could have been compromised (e.g your site-specific password
was divulged, and an attacker tries to brute-force your master passsword with the knowledge that you
might be using SuperGenPass), you should definitely change your master password rather than simply changing the suffix.

### Why is it called Passé?

Well, the idea of using the same password (or a few passwords) for
everything online is pretty much broken and outdated.

And yet there doesn't seem to be a clear way out of this mess.
Passé doesn't try to change the internet, it just acts as a
smart layer between you and your passwords.

Also, Passé shares a lot of letters with "pass" / "password".
It turns out a lot of these sorts of names are already taken, but nobody
else is scrambling for the self-deprecating ones :-)

### I don't trust you!

Good! Depending on how little you trust me, you can do any (or all) of these things:

 - check out the source code on [github.com/gfxmonk/passe](https://github.com/gfxmonk/passe)
 - audit the code to your heart's content
 - build it yourself
 - run your own instance, and never even talk to the public server

<!-- /about -->

# Hacking

Passé is written in Ocaml (both the server and client; which is compiled for the browser using `js_of_ocaml`).

You'll need [opam](http://opam.ocamlpro.com/), and then:

	$ opam install camlp4 batteries yojson ssl cohttp lambda-term uri lwt sha safepass js_of_ocaml
	$ make

	# run the server
	$ ./bin/passe

	# You can now hit up http://localhost:8080/ in a browser,
	# or run the CLI with:
	$ env PASSE_SERVER="http://localhost:8080" ./bin/passe

<a name="run-own"/>

# Run your own server:

If you'd just like to build a server and don't want to install the dependencies yourself,
you can just `make openshift/all`. As long as you have docker,
that should build a binary in openshift/bin/server which should run on any
recent RPM-based distro (fedora/centos/rhel). This includes running on an
openshift DIY container, which is what I do.

# Licence

MIT (see ./LICENSE file).