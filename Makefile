GUP=tools/bin/gup

# Default to building the `all` target
all: phony ${GUP}
	@+${GUP} all

result: phony nix/local.tgz
	nix-build --show-trace

# Catch-all target which delgates to `gup`
%: phony ${GUP}
	@+${GUP} $@

# Remove self-rules
Makefile: ;
${GUP}: ;

# Always rebuild gup targets
.PHONY: phony
