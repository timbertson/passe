{ pkgs }:
with pkgs;
stdenv.mkDerivation {
  name = "libc-null";
  unpackPhase = "true";
  buildInputs = [ gcc pkgconfig ];

  # NOTE: we put the actual library in _lib to prevent anyone accidentally picking up
  # this .so via lib/ naming convention
  buildPhase = ''
    touch libc.c
    cat << EOF > libc-null.pc
      prefix=$out
      libdir=\''${prefix}/_lib

      Name: libc-null
      Version: 0.1
      Description: empty libc.so
      Libs: -L\''${libdir}
    EOF
    gcc -shared -nostdlib -o libc.so libc.c
  '';
  installPhase = ''
    mkdir -p $out/lib/pkgconfig
    mkdir -p $out/_lib
    cp libc-null.pc $out/lib/pkgconfig/
    cp libc.so $out/_lib/
  '';
}
