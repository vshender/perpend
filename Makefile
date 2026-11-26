# The entry point for the commands that cover the whole repository: the core
# and every provider.
#
# A provider is a directory under providers/ that has a Makefile of its own.
# A target here that covers the providers runs the target of the same name
# in each of them.

PROVIDERS := $(patsubst %/Makefile,%,$(wildcard providers/*/Makefile))

.PHONY: check
check: check-core check-providers

.PHONY: check-core
check-core:
	dune build
	dune test

.PHONY: check-providers
check-providers:
	@for provider in $(PROVIDERS); do $(MAKE) -C $$provider check || exit 1; done

.PHONY: clean
clean: clean-core clean-providers

.PHONY: clean-core
clean-core:
	dune clean

.PHONY: clean-providers
clean-providers:
	@for provider in $(PROVIDERS); do $(MAKE) -C $$provider clean || exit 1; done
