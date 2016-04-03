@0xff644b301f762c1c;

using Spk = import "/sandstorm/package.capnp";
const pkgdef :Spk.PackageDefinition = (
  id = "1ntxpncwup71j4912xd1vgn4xgu7eefq9au6yaws092z00zqp3rh",
  manifest = (
    appTitle = (defaultText = "Passe"),
    appVersion = 0,  # Increment this for every release.
    appMarketingVersion = (defaultText = "0.0.0"),
    actions = [
      ( title = (defaultText = "New password store"),
        command = .startPasse
      )
    ],
    continueCommand = .startPasse,

    metadata = (
      icons = (
      ),

      website = "http://example.com",
      codeUrl = "http://example.com",
      license = (none = void),
      categories = [],
      author = (
        contactEmail = "tim@gfxmonk.net",
      ),
      shortDescription = (defaultText = "Password utility"),
      screenshots = [
      ],
    ),
  ),

  sourceMap = (
    searchPath = [
      ( sourcePath = "." ),  # Search this directory first.
      ( sourcePath = "/",    # Then search the system root directory.
        hidePaths = [ "home", "proc", "sys",
                      "etc/passwd", "etc/hosts", "etc/host.conf",
                      "etc/nsswitch.conf", "etc/resolv.conf" ]
      )
    ]
  ),

  fileList = "sandstorm-files.list",
  alwaysInclude = [],
);

const startPasse :Spk.Manifest.Command = (
  argv = ["/sandstorm-http-bridge", "8080", "--", "/opt/app/sandstorm/passe-sandstorm", "--port", "8080", "-vvv"],
  environ = [
    (key = "PATH", value = "/usr/local/bin:/usr/bin:/bin"),
    (key = "APP_ROOT", value = "/opt/app/_build"),
    (key = "SANDSTORM", value = "1"),
  ]
);
