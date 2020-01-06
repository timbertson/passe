GUP=tools/bin/gup

# Default to building the `all` target
_default: phony all

# Catch-all target which delgates to `gup`
%: phony ${GUP}
	@+${GUP} $@

# Remove self-rules
Makefile: ;
${GUP}: ;

# Always rebuild gup targets
.PHONY: phony
